(ns lambdaisland.dbus.format-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [lambdaisland.dbus.format :as format]))

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
