(ns lambdaisland.dbus.format-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [lambdaisland.dbus.format :as format])
  (:import java.nio.ByteOrder))

(deftest basic-type-round-trip-test
  (testing "Basic types round-trip correctly"
    (let [test-cases [[:byte 42]
                      [:bool true]
                      [:bool false]
                      [:string "hello"]
                      [:object-path "/org/test"]
                      [:uint32 12345]
                      [:int32 -42]]]
      (doseq [[type value] test-cases]
        (let [buf (format/byte-buffer)
              _ (format/write-type buf type value)
              _ (.flip buf)
              result (format/read-type buf type)]
          (is (= value result) (str "Type: " type ", Value: " value)))))))

(deftest tuple-round-trip-test
  (testing "Tuples round-trip correctly"
    (let [test-cases [[[:tuple :string :uint32] ["test" 42]]
                      [[:tuple :bool :string :int32] [true "hello" -123]]]]
      (doseq [[type value] test-cases]
        (let [buf (format/byte-buffer)
              _ (format/write-type buf type value)
              _ (.flip buf)
              result (format/read-type buf type)]
          (is (= value result) (str "Type: " type ", Value: " value)))))))

(deftest array-round-trip-test
  (testing "Arrays round-trip correctly"
    (let [test-cases [[[:array :string] ["a" "b" "c"]]
                      [[:array :uint32] [1 2 3 4 5]]
                      [[:array :bool] [true false true]]]]
      (doseq [[type value] test-cases]
        (let [buf (format/byte-buffer)
              _ (format/write-type buf type value)
              _ (.flip buf)
              result (format/read-type buf type)]
          (is (= value result) (str "Type: " type ", Value: " value)))))))

(deftest struct-round-trip-test
  (testing "Structs round-trip correctly"
    (let [test-cases [[[:struct :string :uint32] ["test" 42]]
                      [[:struct :bool :string :int32] [true "hello" -123]]]]
      (doseq [[type value] test-cases]
        (let [buf (format/byte-buffer)
              _ (format/write-type buf type value)
              _ (.flip buf)
              result (format/read-type buf type)]
          (is (= value result) (str "Type: " type ", Value: " value)))))))

(defn write->bytes
  "Write a value with the given type, return the marshalled bytes."
  [type value]
  (let [buf (format/byte-buffer)
        _   (.order buf ByteOrder/LITTLE_ENDIAN)
        _   (format/write-type buf type value)
        _   (.flip buf)
        out (byte-array (.remaining buf))]
    (.get buf out)
    (into [] out)))

(defn bytes->buf [bytes]
  (let [buf (format/byte-buffer)]
    (.order buf ByteOrder/LITTLE_ENDIAN)
    (.put buf (byte-array bytes))
    (.flip buf)
    buf))

(def captured-array-bytes
  "A single-element a(sv): 4-byte length (30) + 4 bytes padding + the
  struct ('Description', variant 's' -> 'hello'). Captured verbatim from
  StartTransientUnit on the wire."
  [30 0 0 0 0 0 0 0 11 0 0 0 68 101 115 99 114 105 112 116 105 111 110 0 1 115 0 0 5 0 0 0 104 101 108 108 111 0])

(deftest array-marshalling-matches-wire-test
  (testing "array of 8-byte-aligned structs matches bytes captured off the wire"
    ;; An empty a(sv)
    (is (= [0 0 0 0 0 0 0 0]
           (write->bytes [:array [:struct :string :variant]] [])))

    (is (= captured-array-bytes
           (write->bytes [:array [:struct :string :variant]] [["Description" "hello"]]))))

  (testing "decoding the wire bytes yields the original value"
    (is (= [] (format/read-type (bytes->buf [0 0 0 0 0 0 0 0]) [:array [:struct :string :variant]])))
    (is (= [["Description" "hello"]]
           (format/read-type (bytes->buf captured-array-bytes) [:array [:struct :string :variant]])))))
