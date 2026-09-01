(ns lambdaisland.dbus.format
  "Read/write the DBUS message format"
  (:require
   [lambdaisland.dbus.platform :as platform])
  (:import
   (clojure.lang BigInt)
   (java.io Reader StringReader)
   (java.nio.charset StandardCharsets)))

(set! *warn-on-reflection* true)

(def ^:dynamic *default-buffer-size* 4096)
(def ^:dynamic *buffer-offset* 0)

(def ^:private uint64-modulus 18446744073709551616)

(defn byte-buffer
  ([] (platform/buffer *default-buffer-size*))
  ([size] (platform/buffer size)))

(defn align [buf size]
  (dotimes [_ (mod (- size (mod (- (platform/position buf) *buffer-offset*) size)) size)]
    (platform/put-byte! buf 0))
  buf)

;; Type reader functions

(defn get-bool [buf]
  (align buf 4)
  (let [v (platform/get-int32 buf)]
    (if (= v 1) true false)))

(defn get-int16 [buf]
  (align buf 2)
  (platform/get-int16 buf))

(defn get-int32 [buf]
  (align buf 4)
  (platform/get-int32 buf))

(defn get-int64 [buf]
  (align buf 8)
  (platform/get-int64 buf))

;; While the DBUS spec considers BYTE an unsigned type, we've decided to
;; read/write it as a Java byte value, meaning signed between -128 and 127
(defn get-byte [buf]
  (platform/get-byte buf))

;; These do read as unsigned, so they always return natural numbers, possibly
;; promoting implicitly to a bigger type
(defn get-uint16 [buf]
  (align buf 2)
  (let [s (platform/get-int16 buf)]
    (if (neg? s)
      (bit-and s 0xffff)
      s)))

(defn get-uint32 [buf]
  (align buf 4)
  (let [i (platform/get-int32 buf)]
    (if (neg? i)
      (bit-and i 0xffffffff)
      i)))

(defn get-uint64 [buf]
  (align buf 8)
  (let [l (platform/get-int64 buf)]
    (if (neg? l)
      (+ l uint64-modulus)
      l)))

(defn get-double [buf]
  (align buf 8)
  (platform/get-double buf))

(defn get-struct [buf read-fns]
  (align buf 8)
  (mapv (fn [f]
          (f buf))
        read-fns))

(defn get-dict-entry [buf read-k read-v]
  (align buf 8)
  [(read-k buf) (read-v buf)])

(defn get-string [buf]
  (align buf 4)
  (let [len (get-uint32 buf)
        res (String. ^bytes (platform/get-bytes buf len)
                     StandardCharsets/UTF_8)]
    (assert (= 0 (platform/get-byte buf))) ;; NULL
    res))

(defn get-signature [buf]
  (let [len (get-byte buf)
        res (String. ^bytes (platform/get-bytes buf len)
                     StandardCharsets/UTF_8)]
    (assert (= 0 (platform/get-byte buf))) ;; NULL
    res))

;; Type writer functions

(defn put-bool [buf v]
  (align buf 4)
  (platform/put-int32! buf (if v 1 0)))

(defn put-int16 [buf v]
  (align buf 2)
  (platform/put-int16! buf (short v)))

(defn put-int32 [buf v]
  (align buf 4)
  (platform/put-int32! buf (int v)))

(defn put-int64 [buf v]
  (align buf 8)
  (platform/put-int64! buf (long v)))

;; Use unchecked variants here so Java implicitly can convert from unsigned to
;; signed through two's complement truncation
(defn put-byte [buf v]
  (platform/put-byte! buf (unchecked-byte v)))

(defn put-uint32 [buf v]
  (align buf 4)
  (platform/put-int32! buf (unchecked-int v)))

(defn put-uint16 [buf v]
  (align buf 2)
  (platform/put-int16! buf (unchecked-short v)))

(defn put-uint64 [buf v]
  (align buf 8)
  (platform/put-int64! buf (unchecked-long v)))

(defn put-double [buf v]
  (align buf 8)
  (platform/put-double! buf (double v)))

(defn put-string [buf s]
  (align buf 4)
  (let [b (.getBytes ^String s)]
    (platform/put-int32! buf (count b))
    (platform/put-bytes! buf b)
    (platform/put-byte! buf 0)))

(defn put-signature [buf s]
  (let [b (.getBytes ^String s)]
    (platform/put-byte! buf (count b))
    (platform/put-bytes! buf b)
    (platform/put-byte! buf 0)))

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

(def message-type->code
  (into {} (map-indexed (fn [i t] [t i]) message-types)))

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

(defn alignment
  "Byte-alignment required by a D-Bus type. STRUCT and DICT_ENTRY are always
  8-byte aligned; ARRAY aligns its 4-byte length; primitives align to their
  natural size; SIGNATURE, BYTE and VARIANT are 1-byte aligned."
  [t]
  (cond
    (vector? t)
    (case (first t)
      (:struct :dict-entry) 8
      :array 4
      1)

    (contains? #{:int64 :uint64 :double} t) 8
    (contains? #{:int32 :uint32 :bool :string :object-path} t) 4
    (contains? #{:int16 :uint16} t) 2

    :else 1))

(defn get-array [buf elem-alignment read-fn]
  (align buf 4)
  (let [len (get-uint32 buf)
        _ (align buf elem-alignment)
        end (+ (platform/position buf) len)]
    (loop [res []]
      (if (< (platform/position buf) end)
        (recur (conj res (read-fn buf)))
        res))))

(defn put-array [buf elem-alignment write-elements-fn]
  (align buf 4)
  (let [size-pos (platform/position buf)]
    (platform/put-int32! buf 0)
    (align buf elem-alignment)
    (let [elem-start (platform/position buf)]
      (write-elements-fn buf)
      (let [end-pos (platform/position buf)]
        (platform/set-position! buf size-pos)
        (platform/put-int32! buf (- end-pos elem-start))
        (platform/set-position! buf end-pos))))
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
               (mapv (fn [t] (read-type buf t)) (rest t))))
           :array
           (fn [buf]
             (let [arr (get-array buf (alignment (second t)) #(read-type % (second t)))]
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

(defn read-header [buf]
  (align buf 8)
  (let [code (get-byte buf)
        ;; _ (prn "<H" (platform/position buf) code)
        _ (assert (< 0 code 10) code)
        [h t] (nth headers code)
        sig (get-signature buf)
        v (read-type buf (sig->type sig))]
    [h v]))

(defn read-message-header [buf]
  (let [endian-char  (char (platform/get-byte buf))
        _ (when-not (#{\l \B} endian-char) (println "BAD ENDIAN" endian-char))
        endian (case endian-char
                 \l :LITTLE_ENDIAN
                 \B :BIG_ENDIAN)
        _ (platform/set-order! buf endian)
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
        headers (into {} (get-array buf 8 read-header))
        #_#_sig (get headers :signature)]
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

(defn read-message [buf]
  (let [{:keys [headers] :as msg} (read-message-header buf)
        {:keys [signature]} headers]
    (if signature
      (assoc msg :body (read-body buf signature))
      msg)))

(defn show-buffer-lim [b]
  (let [p (platform/limit b)]
    (platform/set-position! b 0)
    (repeatedly p #(platform/get-byte b))))

(defn show-buffer-pos [b]
  (let [p (platform/position b)]
    (platform/set-position! b 0)
    (repeatedly p #(platform/get-byte b))))

(defn derive-type [v]
  (cond
    (boolean? v)
    :bool
    (instance? Byte v)
    :byte
    (float? v)
    :double
    (int? v)
    (if (<= Integer/MIN_VALUE v Integer/MAX_VALUE)
      :int32
      :int64)
    (or (instance? BigInt v) (instance? BigInteger v))
    (if (< v 0)
      :int64
      :uint64)
    (string? v)
    :string
    (vector? v)
    (into [:struct] (map derive-type) v)
    (sequential? v)
    [:array (derive-type (first v))]
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
          (put-array buf (alignment (second t))
                     (fn [buf]
                       (doseq [elem v]
                         (write-type buf (second t) elem)))))
        :struct
        (fn [buf v]
          (put-struct buf (rest t) v))
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

(defn write-headers [buf header-map]
  (let [hidx (into {} (map-indexed (fn [idx [k v]] [k idx]) headers))
        headers (into {} headers)]
    (doseq [[k v] header-map]
      (align buf 8)
      (let [t (get headers k)
            code (get hidx k)]
        (put-byte buf code)
        (write-type buf [:variant t] v)))
    buf))

(defn write-message [buf {:keys [endian type flags headers version serial body]
                          :or {version 1
                               endian :LITTLE_ENDIAN}}]
  (platform/set-order! buf endian)
  (put-byte buf (case endian
                  :LITTLE_ENDIAN (byte \l)
                  :BIG_ENDIAN (byte \B)))
  (put-byte buf (message-type->code type))
  (put-byte buf (cond-> 0
                  (:no-reply-expected flags)
                  (bit-or 1)
                  (:no-auto-start flags)
                  (bit-or 2)
                  (:allow-interactive-authorization flags)
                  (bit-or 4)))
  (put-byte buf version)

  (let [body-length-pos (platform/position buf)]
    (put-uint32 buf 0) ;;body length placeholder
    (put-uint32 buf serial)

    (put-array buf 8 #(write-headers % headers))
    (align buf 8)
    (let [body-start-pos (platform/position buf)]
      (when-let [sig (get headers :signature)]
        (write-type buf (sig->type (get headers :signature)) body))
      (let [body-end-pos (platform/position buf)
            body-len (- body-end-pos body-start-pos)]
        (platform/put-int32-at! buf body-length-pos body-len)
        (platform/set-position! buf body-end-pos))))
  buf)

(defn write-to-str
  "Testing utility, takes a function that takes a buffer as its first argument,
  and any arguments to that function, calls it on a new buffer, then converts to
  String."
  [f & args]
  (let [b (byte-buffer)]
    (apply f b args)
    (let [len (platform/position b)]
      (platform/flip! b)
      (String. ^bytes (platform/get-bytes b len) StandardCharsets/UTF_8))))

(comment

  (let [v {"foo" 123 "bar" "x"}
        t (derive-type v)
        b (byte-buffer)]
    (write-type b t v)
    (platform/flip! b)
    (read-type b t)
    )
  )
