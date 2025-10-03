(ns lambdaisland.dbus.generative-tests
  (:require
   [clojure.string :as str]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [lambdaisland.dbus.format :as format])
  (:import
   (java.nio ByteOrder)))

(def byte-gen gen/byte)
(def uint16-gen (gen/choose 0 (long (dec (Math/pow 2 16)))))
(def uint32-gen (gen/choose 0 (long (dec (Math/pow 2 32)))))
(def uint64-gen
  (gen/one-of
   [(gen/choose 0 Long/MAX_VALUE)
    (gen/fmap #(bigint (Long/toUnsignedString %))
              (gen/choose Long/MIN_VALUE -1))]))

(def int16-gen
  (gen/choose Short/MIN_VALUE Short/MAX_VALUE))

(def int32-gen
  (gen/choose Integer/MIN_VALUE Integer/MAX_VALUE))

(def int64-gen
  (gen/choose Long/MIN_VALUE Long/MAX_VALUE))

(def double-gen gen/double)
(def bool-gen gen/boolean)
(def string-gen (gen/not-empty gen/string-alphanumeric))

(def object-path-gen
  (gen/fmap #(str "/" (clojure.string/replace % #"[^a-zA-Z0-9_]" "_"))
            (gen/not-empty gen/string-alphanumeric)))

(def signature-gen
  (gen/fmap #(apply str %)
            (gen/vector (gen/elements [\y \b \n \q \i \u \x \t \d \s \o \g \a \v \( \)])
                        1 10)))

;; Member names: [A-Za-z0-9_]+, no digits at start, no periods, 1-255 chars
(def member-name-gen
  (gen/fmap #(apply str %)
            (gen/vector
             (gen/one-of
              [(gen/elements (map char (concat (range 65 91) (range 97 123) [95]))) ; A-Za-z_
               (gen/elements (map char (range 48 58)))]) ; 0-9
             1 255)))

;; Interface names: 2+ elements separated by periods, each element [A-Za-z0-9_]+,
;; no digits at start, 1-255 chars total
(def interface-name-gen
  (let [element-gen (gen/fmap #(apply str %)
                              (gen/vector
                               (gen/one-of
                                [(gen/elements (map char (concat (range 65 91) (range 97 123) [95]))) ; A-Za-z_
                                 (gen/elements (map char (range 48 58)))]) ; 0-9
                               1 25))]
    (gen/fmap #(str/join "." %)
              (gen/vector element-gen 2 10))))

;; Recursive generator for DBus types
(def atomic-type-gen
  (gen/elements
   [:byte :bool :double :string
    :int16 :int32 :int64
    :uint16 :uint32 :uint64]))

(def dbus-type-gen
  (gen/recursive-gen
   (fn [inner-gen]
     (gen/one-of
      [atomic-type-gen
       (gen/tuple (gen/return :array)
                  inner-gen)
       (gen/tuple (gen/return :variant)
                  inner-gen)
       (gen/bind (gen/choose 1 6)
                 (fn [n]
                   (apply gen/tuple (gen/return :struct) (repeat n inner-gen))))]))
   atomic-type-gen))

(defn type->value-gen [t]
  (cond
    (keyword? t)
    (case t
      :byte byte-gen
      :bool bool-gen
      :string string-gen
      :double double-gen
      :int16 int16-gen
      :int32 int32-gen
      :int64 int64-gen
      :uint16 uint16-gen
      :uint32 uint32-gen
      :uint64 uint64-gen)

    (vector? t)
    (case (first t)
      :array (gen/vector (type->value-gen (second t)) 0 5)
      :variant (type->value-gen (second t))
      :struct (apply gen/tuple (map type->value-gen (rest t))))))

(def dbus-value-gen
  (gen/bind dbus-type-gen type->value-gen))

(def flags-gen
  (gen/map (gen/elements [:no-reply-expected :no-auto-start :allow-interactive-authorization])
           (gen/return true)))

(def message-call-gen
  (gen/bind
   dbus-type-gen
   (fn [t]
     (gen/hash-map
      :endian (gen/elements [:LITTLE_ENDIAN :BIG_ENDIAN])
      :type (gen/return :method-call)
      :flags flags-gen
      :version (gen/return 1)
      :body-length gen/nat
      :serial gen/nat
      :headers (gen/hash-map
                :signature (gen/return (format/type->sig t))
                :path object-path-gen
                :interface interface-name-gen
                :member member-name-gen)
      :body (type->value-gen t)))))

(defn round-trip [message]
  (let [buf (format/byte-buffer)
        _ (format/write-message buf message)
        _ (.flip buf)]
    (format/read-message buf)))

(defn eq
  "Normal clojure equality does not consider ##NaN equal to ##NaN, which prevents
  us from checking that NaN round trips."
  [this that]
  (or (= this that)
      (and (number? this) (NaN? this) (number? that) (NaN? that))
      (and (sequential? this) (sequential? that)
           (= (count this) (count that))
           (every? identity (map eq this that)))
      (and (map? this) (map? that)
           (= (count this) (count that))
           (every? identity (map #(eq (get this %)
                                      (get that %))
                                 (distinct
                                  (concat (keys this)
                                          (keys that))))))))

(defn round-trip? [message]
  (let [read-message (round-trip message)]
    (eq (dissoc message :body-length)
        (dissoc read-message :body-length))))

(defspec round-trip-property 100
  (prop/for-all [message message-call-gen]
                (try
                  (round-trip? message)
                  (catch Throwable t
                    false))))
