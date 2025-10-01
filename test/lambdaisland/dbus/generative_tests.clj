(ns lambdaisland.dbus.generative-tests
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.test.check :as tc]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [clojure.test.check.clojure-test :refer [defspec]]
   [lambdaisland.dbus.format :as format]))

(def byte-gen gen/byte)
(def uint16-gen (gen/choose 0 (long (dec (Math/pow 2 16)))))
(def uint32-gen (gen/choose 0 (long (dec (Math/pow 2 32)))))
(def uint64-gen (gen/fmap Math/abs gen/large-integer))

(def int16-gen
  (let [n (long (Math/pow 2 15))]
    (gen/choose (- (inc n)) (dec n))))

(def int32-gen
  (let [n (long (Math/pow 2 32))]
    (gen/choose (- (inc n)) (dec n))))

(def int64-gen gen/large-integer)
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

;; Recursive generator for DBus types
(def dbus-type-gen
  (gen/recursive-gen
   (fn [inner-gen]
     (gen/one-of
      [(gen/return :byte)
       (gen/return :bool)
       (gen/return :double)
       (gen/return :int16)
       (gen/return :int32)
       (gen/return :int64)
       (gen/return :object-path)
       (gen/return :signature)
       (gen/return :string)
       (gen/return :uint16)
       (gen/return :uint32)
       (gen/return :uint64)
       (gen/fmap (fn [_] [:array inner-gen]) (gen/return nil))
       (gen/fmap (fn [_] [:variant inner-gen]) (gen/return nil))
       (gen/fmap (fn [types] (into [:struct] types))
                 (gen/vector inner-gen 1 4))]))
   (gen/elements [:byte :bool :double :int16 :int32 :int64 :object-path
                  :signature :string :uint16 :uint32 :uint64])))

(defn type->value-gen [t]
  (cond
    (keyword? t)
    (case t
      :byte byte-gen
      :bool bool-gen
      :double double-gen
      :int16 int16-gen
      :int32 int32-gen
      :int64 int64-gen
      :object-path object-path-gen
      :signature signature-gen
      :string string-gen
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

;; Generator for DBus messages
(def message-type-gen
  (gen/elements [:method-call :method-return :error :signal]))

(def flags-gen
  (gen/map (gen/elements [:no-reply-expected :no-auto-start :allow-interactive-authorization])
           gen/boolean))

(def headers-gen
  (gen/let [header-keys (gen/vector (gen/elements [:path :interface :member :error-name :reply-serial
                                                   :destination :sender :signature :unix-fds])
                                    3 9)]
    (gen/fmap
     (fn [header-values]
       (zipmap header-keys header-values))
     (gen/tuple
      (gen/one-of (repeat (count header-keys)
                          (gen/one-of [object-path-gen string-gen string-gen string-gen
                                       uint32-gen string-gen string-gen signature-gen uint32-gen])))))))

(def dbus-message-gen
  (gen/hash-map
   :endian (gen/elements [\l \B])
   :type message-type-gen
   :flags flags-gen
   :version (gen/choose 1 2)
   :body-length gen/nat
   :serial gen/nat
   :headers headers-gen
   :body dbus-value-gen))

(comment
  ;; Property tests
  (defspec round-trip-property 100
    (prop/for-all [message dbus-message-gen]
                  (let [buf (format/byte-buffer)
                        _ (format/write-message buf message)
                        _ (.flip buf)
                        read-message (format/read-message buf)]
                    (= (dissoc message :body-length)
                       (dissoc read-message :body-length)))))

  (test #'round-trip-property))
