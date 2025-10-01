(ns lambdaisland.dbus.format
  "Read/write the DBUS message format"
  (:require
   [clojure.java.io :as io])
  (:import
   (java.net ServerSocket StandardProtocolFamily UnixDomainSocketAddress)
   (java.nio ByteBuffer ByteOrder)
   (java.nio.channels ServerSocketChannel SocketChannel)))

(set! *warn-on-reflection* true)

(def ^:dynamic *default-buffer-size* 4096)

(defn byte-buffer ^ByteBuffer []
  (ByteBuffer/allocate *default-buffer-size*))

(declare get-array get-byte get-signature get-string get-string
         put-array put-byte put-signature put-string put-string
         get-bool get-double get-int16 get-int32 get-int64
         put-bool put-double put-int16 put-int32 put-int64
         get-uint16 get-uint32 get-uint64
         put-uint16 put-uint32 put-uint64
         sig->type)

(def types
  [{:id :bool :sig \b :read #'get-bool :write #'put-bool}
   {:id :byte :sig \y :read #'get-byte :write #'put-byte}
   {:id :double :sig \d :read #'get-double :write #'put-double}
   {:id :int16 :sig \n :read #'get-int16 :write #'put-int16}
   {:id :int32 :sig \i :read #'get-int32 :write #'put-int32}
   {:id :int64 :sig \x :read #'get-int64 :write #'put-int64}
   {:id :object-path :sig \o :read #'get-string :write #'put-string}
   {:id :signature :sig \g :read #'get-signature :write #'put-signature}
   {:id :string :sig \s :read #'get-string :write #'put-string}
   {:id :uint16 :sig \q :read #'get-uint16 :write #'put-uint16}
   {:id :uint32 :sig \u :read #'get-uint32 :write #'put-uint32}
   {:id :uint64 :sig \t :read #'get-uint64 :write #'put-uint64}
   {:id :array :sig \a :read #'get-array :write #'put-array}
   {:id :variant :sig \v}
   {:id :struct :sig \( :sig-end\)}])

(def message-types
  [:invalid :method-call :method-return :error :signal])

(def headers
  [[:invalid nil]
   [:path :object-path]
   [:interface :string]
   [:member :string]
   [:error-name :string]
   [:reply-serial :uint32]
   [:destination :string]
   [:sender :string]
   [:signature :signature]
   [:unix-fds :uint32]])

(def sig->type* (into {} (map (juxt :sig :id)) types))
(def type->sig* (into {} (map (juxt :id :sig)) types))
(def type->write-fn* (into {} (map (juxt :id :write)) types))
(def type->read-fn* (into {} (map (juxt :id :read)) types))

(def offset 0)

(defn align [^ByteBuffer buf size]
  (dotimes [_ (mod (- size (mod (- (.position buf) offset) size)) size)]
    (.put buf (byte 0)))
  buf)

(defn get-byte [^ByteBuffer buf]
  (bit-and (.get buf) 0xff))

(defn get-uint32 [^ByteBuffer buf]
  (align buf 4)
  (bit-and (.getInt buf) 0xffffffff))

(defn get-array [^ByteBuffer buf read-fn]
  (align buf 4)
  (let [len (get-uint32 buf)
        end (+ (.position buf) len)]
    (loop [res []]
      (if (< (.position buf) end)
        (recur (conj res (read-fn buf)))
        res))))

(defn get-struct [^ByteBuffer buf read-fns]
  (align buf 8)
  (mapv #(% buf) read-fns))

(defn get-string [^ByteBuffer buf]
  (align buf 4)
  (let [len (get-uint32 buf)
        res (String. ^bytes (let [ba (byte-array len)]
                              (.get buf ba 0 len)
                              ba))]
    (.get buf) ;; NULL
    res))

(defn get-signature [^ByteBuffer buf]
  (let [len (get-byte buf)
        res (String. ^bytes (let [ba (byte-array len)]
                              (.get buf ba 0 len)
                              ba))]
    (.get buf) ;; NULL
    res))

(defn read-type [buf t]
  ((or (type->read-fn* t)
       (cond
         (vector? t)
         (case (first t)
           :tuple
           (if (= 1 (count t))
             (constantly nil)
             (fn [buf]
               (for [t (rest t)]
                 (read-type buf t))))
           :array
           (fn [buf]
             (get-array buf #(read-type % (second t))))
           :struct
           (fn [buf]
             (get-struct buf (map (fn [t] #(read-type % t)) (rest t))))
           :variant
           (fn [buf]
             (let [sig (get-signature buf)]
               (read-type buf (sig->type sig)))))

         :else
         (throw (ex-info  "unimplemented type" {:t t}))))
   buf))

(defn type->sig [t]
  (if (vector? t)
    (case (first t)
      :tuple
      (apply str (map type->sig (rest t)))
      :array
      (str "a" (type->sig (second t)))
      :struct
      (str "(" (apply str (map type->sig (rest t))) ")")
      :variant
      "v")
    (str (type->sig* t))))

(defn sig->type [sig]
  (if (char? sig)
    (sig->type* sig)
    (loop [t [:tuple]
           [c & cs] sig]
      (if (not c)
        t
        (case c
          \(
          (recur
           (conj t
                 (into [:struct] (next (sig->type
                                        (take-while (complement #{\)}) cs)))))
           (next (drop-while (complement #{\)}) cs)))
          \a
          (recur
           (conj t [:array (sig->type (first cs))])
           (next cs))

          \v
          (let [[_tuple & ts] (sig->type cs)]
            (into (conj t [:variant (first ts)]) (next ts)))

          #_else
          (recur (conj t (sig->type c))
                 cs))))))

(defn read-header [^ByteBuffer buf]
  (align buf 8)
  (let [code (get-byte buf)
        ;; _ (prn "<H" (.position buf) code)
        _ (assert (< 0 code 10) code)
        [h t] (nth headers code)
        sig (get-signature buf)
        v (read-type buf (sig->type sig))]
    ;; (prn [(.position buf) h t v])
    [h v]))

(defn read-message [^ByteBuffer buf]
  (let [endian (.get buf)
        _ (.order buf
                  (case (char endian)
                    \l
                    ByteOrder/LITTLE_ENDIAN
                    \B
                    ByteOrder/BIG_ENDIAN))
        msg-type (nth message-types (get-byte buf))
        flags (let [flags (get-byte buf)]
                (cond-> {}
                  (= 0x1 (bit-and flags 0x1))
                  (assoc :no-reply-expected true)
                  (= 0x2 (bit-and flags 0x2))
                  (assoc :no-auto-start true)
                  (= 0x4 (bit-and flags 0x4))
                  (assoc :allow-interactive-authorization true)))
        version (get-byte buf)
        len  (get-uint32 buf)
        serial  (get-uint32 buf)
        headers (into {} (get-array buf read-header))]
    {:endian (char endian)
     :type msg-type
     :flags flags
     :version version
     :body-length len
     :serial serial
     :headers headers
     :body (read-type buf (sig->type (get headers :signature)))}))

(defn put-string [^ByteBuffer buf s]
  (align buf 4)
  (let [b (.getBytes ^String s)]
    (.putInt buf (count b))
    (.put buf b)
    (.put buf (byte 0))))

(defn put-signature [^ByteBuffer buf s]
  (let [b (.getBytes ^String s)]
    (.put buf (byte (count b)))
    (.put buf b)
    (.put buf (byte 0))))

(defn put-byte [^ByteBuffer buf v]
  (.put buf (byte (bit-and (long v) 0xff))))

(defn put-uint32 [^ByteBuffer buf v]
  (align buf 4)
  (.putInt buf (int (bit-and (long v) 0xffffffff))))

(defn get-bool [^ByteBuffer buf]
  (align buf 4)
  (let [v (.getInt buf)]
    (if (= v 1) true false)))

(defn get-int16 [^ByteBuffer buf]
  (align buf 2)
  (.getShort buf))

(defn get-int32 [^ByteBuffer buf]
  (align buf 4)
  (.getInt buf))

(defn get-int64 [^ByteBuffer buf]
  (align buf 8)
  (.getLong buf))

(defn get-uint16 [^ByteBuffer buf]
  (align buf 2)
  (bit-and (.getShort buf) 0xffff))

(defn get-uint64 [^ByteBuffer buf]
  (align buf 8)
  (bit-and (.getLong buf) 0xffffffffffffffff))

(defn get-double [^ByteBuffer buf]
  (align buf 8)
  (.getDouble buf))

(defn put-bool [^ByteBuffer buf v]
  (align buf 4)
  (.putInt buf (if v 1 0)))

(defn put-int16 [^ByteBuffer buf v]
  (align buf 2)
  (.putShort buf (short v)))

(defn put-int32 [^ByteBuffer buf v]
  (align buf 4)
  (.putInt buf (int v)))

(defn put-int64 [^ByteBuffer buf v]
  (align buf 8)
  (.putLong buf (long v)))

(defn put-uint16 [^ByteBuffer buf v]
  (align buf 2)
  (.putShort buf (short (bit-and (long v) 0xffff))))

(defn put-uint64 [^ByteBuffer buf v]
  (align buf 8)
  (.putLong buf (bit-and (long v) 0xffffffffffffffff)))

(defn put-double [^ByteBuffer buf v]
  (align buf 8)
  (.putDouble buf (double v)))

(defn write-array [^ByteBuffer buf write-elements-fn]
  (align buf 4)
  (let [size-pos (.position buf)]
    (.putInt buf 0)
    (write-elements-fn buf)
    (let [end-pos (.position buf)]
      (.position buf size-pos)
      (.putInt buf (- end-pos size-pos 4))
      (.position buf end-pos)))
  buf)

(defn show-buffer-lim [^ByteBuffer b]
  (let [p (.limit b)]
    (.position b 0)
    (repeatedly p #(.get b))))

(defn show-buffer-pos [^ByteBuffer b]
  (let [p (.position b)]
    (.position b 0)
    (repeatedly p #(.get b))))

(defn write-type [buf t v]
  ((or
    (type->write-fn* t)
    (cond
      (vector? t)
      (case (first t)
        :tuple
        (if (= 1 (count t))
          (constantly nil)
          (fn [buf vs]
            (doall
             (map (partial write-type buf) (rest t) vs))))
        :array
        (fn [buf v]
          (write-array buf
                      (fn [buf]
                        (doseq [elem v]
                          (write-type buf (second t) elem)))))
        :struct
        (fn [buf vs]
          (doall
           (map (partial write-type buf) (rest t) vs)))
        :variant
        (fn [buf v]
          (put-signature buf (type->sig (second t)))
          (write-type buf (second t) v)))

      :else
      (throw (ex-info  "unimplemented type" {:t t}) )))
   buf v))

(defn write-headers [^ByteBuffer buf header-map]
  (let [hidx (into {} (map-indexed (fn [idx [k v]] [k idx]) headers))
        headers (into {} headers)]
    (doseq [[k v] header-map]
      (align buf 8)
      (let [t (get headers k)
            code (get hidx k)]
        (println ["H" k code t v (.position buf)])
        (put-byte buf code)
        (write-type buf [:variant t] v)))
    buf))

(defn write-message [^ByteBuffer buf {:keys [type flags headers version serial body]
                                      :or {version 1}}]
  (let [endian (.order buf)]
    (put-byte buf (if (= endian ByteOrder/LITTLE_ENDIAN)
                    \l
                    \B))
    (put-byte buf (.indexOf ^java.util.List message-types type))
    (put-byte buf (cond-> 0
                    (:no-reply-expected flags)
                    (bit-or 1)
                    (:no-auto-start flags)
                    (bit-or 2)
                    (:allow-interactive-authorization flags)
                    (bit-or 3)))
    (put-byte buf version)

    (let [body-length-pos (.position buf)]
      (put-uint32 buf 0) ;;body length placeholder
      (put-uint32 buf serial)

      (write-array buf #(write-headers % headers))
      (align buf 8)

      (let [body-start-pos (.position buf)]
        (write-type buf (sig->type (get headers :signature)) body)
        (let [body-end-pos (.position buf)
              body-len (- body-end-pos body-start-pos)]
          (.putInt buf body-length-pos body-len))))
    buf))

(defn sock-read ^bytes [^SocketChannel chan]
  (let [buf (byte-buffer)]
    (.mark buf)
    (let [len (.read chan buf)]
      (if (< 0 len)
        (let [arr (byte-array len)]
          (.flip buf)
          (.get buf arr 0 len)
          arr)
        (do
          (println "WARN: read from closed channel")
          (byte-array 0))))))

(defn sock-write
  ([^SocketChannel chan ^String s]
   (let [buf (byte-buffer)]
     (.mark buf)
     (.put buf (.getBytes s))
     (.flip buf)
     (.write chan buf)))
  ([^SocketChannel chan f & args]
   (let [buf (byte-buffer)]
     (.mark buf)
     (apply f buf args)
     (.flip buf)
     (.write chan buf))))

(defn write-to-str
  "Testing utility, takes a function that takes a buffer as its first argument,
  and any arguments to that function, calls it on a new buffer, then converts to
  String."
  [f & args]
  (let [^ByteBuffer b (byte-buffer)]
    (.mark b)
    (apply f b args)
    (let [len (.position b)
          arr (byte-array len)]
      (.flip b)
      (.get b arr 0 len)
      (String. arr))))

(defn sock-conn ^SocketChannel [^String sock-loc]
  (SocketChannel/open (UnixDomainSocketAddress/of sock-loc)))

(defn session-sock []
  (let [[_ path] (re-find #"unix:path=(.*)" (System/getenv "DBUS_SESSION_BUS_ADDRESS"))]
    (sock-conn path)))

(defn system-sock []
  (sock-conn "/run/dbus/system_bus_socket"))
