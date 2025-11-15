(ns lambdaisland.dbus.format
  "Read/write the DBUS message format"
  (:import
   (clojure.lang BigInt)
   (java.io Reader StringReader)
   (java.nio ByteBuffer ByteOrder)
   (java.nio.charset StandardCharsets)))

(set! *warn-on-reflection* true)

(def ^:dynamic *default-buffer-size* 4096)
(def ^:dynamic *buffer-offset* 0)

(defn byte-buffer
  (^ByteBuffer []
   (byte-buffer *default-buffer-size*))
  (^ByteBuffer [size]
   (ByteBuffer/allocate size)))

(defn align [^ByteBuffer buf size]
  (dotimes [_ (mod (- size (mod (- (.position buf) *buffer-offset*) size)) size)]
    (.put buf (byte 0)))
  buf)

;; Type reader functions

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

;; While the DBUS spec considers BYTE an unsigned type, we've decided to
;; read/write it as a Java byte value, meaning signed between -128 and 127
(defn get-byte [^ByteBuffer buf]
  (.get buf))

;; These do read as unsigned, so they always return natural numbers, possibly
;; promoting implicitly to a bigger type
(defn get-uint16 [^ByteBuffer buf]
  (align buf 2)
  (let [s (.getShort buf)]
    (if (neg? s)
      (bit-and s 0xffff)
      s)))

(defn get-uint32 [^ByteBuffer buf]
  (align buf 4)
  (let [i (.getInt buf)]
    (if (neg? i)
      (bit-and i 0xffffffff)
      i)))

(defn get-uint64 [^ByteBuffer buf]
  (align buf 8)
  (let [l (.getLong buf)]
    (if (neg? l)
      (bigint (java.lang.Long/toUnsignedString l))
      l)))

(defn get-double [^ByteBuffer buf]
  (align buf 8)
  (.getDouble buf))

(defn get-struct [^ByteBuffer buf read-fns]
  (align buf 8)
  (mapv (fn [f]
          (f buf))
        read-fns))

(defn get-dict-entry [^ByteBuffer buf read-k read-v]
  (align buf 8)
  [(read-k buf) (read-v buf)])

(defn get-string [^ByteBuffer buf]
  (align buf 4)
  (let [len (get-uint32 buf)
        res (String. ^bytes (let [ba (byte-array len)]
                              (.get buf ba 0 len)
                              ba)
                     StandardCharsets/UTF_8)]
    (assert (= 0 (.get buf))) ;; NULL
    res))

(defn get-signature [^ByteBuffer buf]
  (let [len (get-byte buf)
        res (String. ^bytes (let [ba (byte-array len)]
                              (.get buf ba 0 len)
                              ba)
                     StandardCharsets/UTF_8)]
    (assert (= 0 (.get buf))) ;; NULL
    res))

;; Type writer functions

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

;; Use unchecked variants here so Java implicitly can convert from unsigned to
;; signed through two's complement truncation
(defn put-byte [^ByteBuffer buf v]
  (.put buf (unchecked-byte v)))

(defn put-uint32 [^ByteBuffer buf v]
  (align buf 4)
  (.putInt buf (unchecked-int v)))

(defn put-uint16 [^ByteBuffer buf v]
  (align buf 2)
  (.putShort buf (unchecked-short v)))

(defn put-uint64 [^ByteBuffer buf v]
  (align buf 8)
  (.putLong buf (unchecked-long v)))

(defn put-double [^ByteBuffer buf v]
  (align buf 8)
  (.putDouble buf (double v)))

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

(def types
  [{:id :bool :sig \b :read get-bool :write put-bool}
   {:id :byte :sig \y :read get-byte :write put-byte}
   {:id :double :sig \d :read get-double :write put-double}
   {:id :int16 :sig \n :read get-int16 :write put-int16}
   {:id :int32 :sig \i :read get-int32 :write put-int32}
   {:id :int64 :sig \x :read get-int64 :write put-int64}
   {:id :object-path :sig \o :read get-string :write put-string}
   {:id :signature :sig \g :read get-signature :write put-signature}
   {:id :string :sig \s :read get-string :write put-string}
   {:id :uint16 :sig \q :read get-uint16 :write put-uint16}
   {:id :uint32 :sig \u :read get-uint32 :write put-uint32}
   {:id :uint64 :sig \t :read get-uint64 :write put-uint64}
   {:id :array :sig \a}
   {:id :variant :sig \v}
   {:id :struct :sig \( :sig-end\)}
   {:id :dict-entry :sig \{ :sig-end\}}])

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

(defn type->sig [t]
  (if (vector? t)
    (case (first t)
      :tuple
      (apply str (map type->sig (rest t)))
      :array
      (str "a" (type->sig (second t)))
      :struct
      (str "(" (apply str (map type->sig (rest t))) ")")
      :dict-entry
      (str "{" (type->sig (nth t 1)) (type->sig (nth t 2)) ")")
      :variant
      "v")
    (str (type->sig* t))))

(declare read-sig)

(defn read-struct-sig [rdr]
  (loop [res [:struct]
         t (read-sig rdr)]
    (if (= :close-struct t)
      res
      (recur (conj res t) (read-sig rdr)))))

(defn read-sig [^Reader rdr]
  (let [i (.read rdr)]
    (when-not (= -1 i)
      (let [ch (char i)]
        (case ch
          \(
          (read-struct-sig rdr)
          \)
          :close-struct
          \{
          (let [t [:dict-entry (read-sig rdr) (read-sig rdr)]]
            (assert (= \} (char (.read rdr))))
            t)
          \a
          [:array (read-sig rdr)]
          (sig->type* ch))))))

(defn sig->type [sig]
  (let [rdr (StringReader. (str sig))]
    (loop [ts []]
      (if-let [t (read-sig rdr)]
        (recur (conj ts t))
        (case (count ts)
          0 nil
          1 (first ts)
          (into [:tuple] ts))))))

(defn get-array [^ByteBuffer buf read-fn]
  (align buf 4)
  (let [len (get-uint32 buf)
        end (+ (.position buf) len)]
    (loop [res []]
      (if (< (.position buf) end)
        (recur (conj res (read-fn buf)))
        res))))

(defn put-array [^ByteBuffer buf write-elements-fn]
  (align buf 4)
  (let [size-pos (.position buf)]
    (.putInt buf 0)
    (write-elements-fn buf)
    (let [end-pos (.position buf)]
      (.position buf size-pos)
      (.putInt buf (- end-pos size-pos 4))
      (.position buf end-pos)))
  buf)

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
             (let [arr (get-array buf #(read-type % (second t)))]
               (if (and (vector? (second t))
                        (= :dict-entry (first (second t))))
                 (into {} arr)
                 arr)))
           :struct
           (fn [buf]
             (get-struct buf (map (fn [t] #(read-type % t)) (rest t))))
           :dict-entry
           (fn [buf]
             (get-dict-entry buf #(read-type % (nth t 1)) #(read-type % (nth t 2)))))

         (= t :variant)
         (fn [buf]
           (let [sig (get-signature buf)]
             (read-type buf (sig->type sig))))

         :else
         (throw (ex-info  "unimplemented type" {:t t}))))
   buf))

(defn read-header [^ByteBuffer buf]
  (align buf 8)
  (let [code (get-byte buf)
        ;; _ (prn "<H" (.position buf) code)
        _ (assert (< 0 code 10) code)
        [h t] (nth headers code)
        sig (get-signature buf)
        v (read-type buf (sig->type sig))]
    [h v]))

(defn byte-order [endian]
  (case endian
    :LITTLE_ENDIAN ByteOrder/LITTLE_ENDIAN
    :BIG_ENDIAN    ByteOrder/BIG_ENDIAN))

(defn read-message-header [^ByteBuffer buf]
  (let [endian (case (char (.get buf))
                 \l :LITTLE_ENDIAN
                 \B :BIG_ENDIAN)
        _ (.order buf (byte-order endian))
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
        headers (into {} (get-array buf read-header))
        sig (get headers :signature)]
    (align buf 8)
    {:endian endian
     :type msg-type
     :flags flags
     :version version
     :body-length len
     :serial serial
     :headers headers}))

(defn read-body [buffer sig]
  (read-type buffer (sig->type sig)))

(defn read-message [^ByteBuffer buf]
  (let [msg (read-message-header buf)
        {:keys [sig]} msg]
    (if sig
      (assoc msg :body (read-body buf sig))
      msg)))

(defn show-buffer-lim [^ByteBuffer b]
  (let [p (.limit b)]
    (.position b 0)
    (repeatedly p #(.get b))))

(defn show-buffer-pos [^ByteBuffer b]
  (let [p (.position b)]
    (.position b 0)
    (repeatedly p #(.get b))))

(defn derive-type [v]
  (cond
    (boolean? v)
    :bool
    (instance? Byte v)
    :byte
    (float? v)
    :double
    (int? v)
    :int64
    (or (instance? BigInt v) (instance? BigInteger v))
    (if (< v 0)
      :int64
      :uint64)
    (string? v)
    :string
    (vector? v)
    (into [:struct] (map derive-type) v)
    (map? v)
    (let [kts (map derive-type (keys v))
          vts (map derive-type (vals v))
          kt (if (and (seq kts) (apply = kts)) (first kts) :variant)
          vt (if (and (seq vts) (apply = vts)) (first vts) :variant)]
      [:array [:dict-entry kt vt]])
    :else
    (throw (ex-info "Can't derive type" {:v v}))))

(declare put-struct)

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
          (put-array buf
                     (fn [buf]
                       (doseq [elem v]
                         (write-type buf (second t) elem)))))
        :struct
        #(put-struct buf (rest t) v)
        :dict-entry
        (fn [buf [k v]]
          (align buf 8)
          (write-type buf (nth t 1) k)
          (write-type buf (nth t 2) v))
        :variant
        (fn [buf v]
          (put-signature buf (type->sig (second t)))
          (write-type buf (second t) v)))

      (= t :variant)
      (fn [buf v]
        (let [t (derive-type v)]
          (put-signature buf (type->sig t))
          (write-type buf t v)))

      :else
      (throw (ex-info  "unimplemented type" {:t t}) )))
   buf v))

(defn put-struct [buf ts vs]
  (align buf 8)
  (doall
   (map (fn [t v]
          (write-type buf t v))
        ts vs)))

(defn write-headers [^ByteBuffer buf header-map]
  (let [hidx (into {} (map-indexed (fn [idx [k v]] [k idx]) headers))
        headers (into {} headers)]
    (doseq [[k v] header-map]
      (align buf 8)
      (let [t (get headers k)
            code (get hidx k)]
        (put-byte buf code)
        (write-type buf [:variant t] v)))
    buf))

(defn write-message [^ByteBuffer buf {:keys [endian type flags headers version serial body]
                                      :or {version 1
                                           endian :LITTLE_ENDIAN}}]
  (.order buf (byte-order endian))
  (put-byte buf (case endian
                  :LITTLE_ENDIAN (byte \l)
                  :BIG_ENDIAN (byte \B)))
  (put-byte buf (.indexOf ^java.util.List message-types type))
  (put-byte buf (cond-> 0
                  (:no-reply-expected flags)
                  (bit-or 1)
                  (:no-auto-start flags)
                  (bit-or 2)
                  (:allow-interactive-authorization flags)
                  (bit-or 4)))
  (put-byte buf version)

  (let [body-length-pos (.position buf)]
    (put-uint32 buf 0) ;;body length placeholder
    (put-uint32 buf serial)

    (put-array buf #(write-headers % headers))
    (align buf 8)
    (let [body-start-pos (.position buf)]
      (when-let [sig (get headers :signature)]
        (write-type buf (sig->type (get headers :signature)) body))
      (let [body-end-pos (.position buf)
            body-len (- body-end-pos body-start-pos)]
        (.putInt buf body-length-pos body-len)
        (.position buf body-end-pos))))
  buf)

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
      (String. arr StandardCharsets/UTF_8))))

(comment

  (let [v {"foo" 123 "bar" "x"}
        t (derive-type v)
        b (byte-buffer)]
    (write-type b t v)
    (.flip b)
    (read-type b t)
    )
  )
