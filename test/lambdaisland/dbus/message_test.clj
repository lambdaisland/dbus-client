(ns lambdaisland.dbus.message-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [lambdaisland.dbus.message :as message]))

(def error-msg
  {:type :error
   :headers {:error-name "org.freedesktop.DBus.Error.UnknownMethod"}
   :body ["No such method" "extra-arg"]})

(def return-msg
  {:type :method-return
   :headers {:reply-serial 1}
   :body ["hello"]})

(deftest error?-test
  (testing "identifies error messages"
    (is (true? (message/error? error-msg)))
    (is (false? (message/error? return-msg)))
    (is (false? (message/error? {:type :signal})))))

(deftest error-name-test
  (testing "extracts the error name from headers"
    (is (= "org.freedesktop.DBus.Error.UnknownMethod" (message/error-name error-msg)))
    (is (nil? (message/error-name return-msg)))))

(deftest ?ex-test
  (testing "returns nil for non-error messages"
    (is (nil? (message/?ex return-msg))))
  (testing "returns an ex-info for error messages"
    (let [ex (message/?ex error-msg)]
      (is (instance? clojure.lang.ExceptionInfo ex))
      (is (= :lambdaisland.dbus/error (:type (ex-data ex))))
      (is (= "org.freedesktop.DBus.Error.UnknownMethod" (:error-name (ex-data ex))))
      (is (= ["No such method" "extra-arg"] (:error-message (ex-data ex))))
      (is (= error-msg (:message (ex-data ex))))
      (is (re-find #"UnknownMethod" (ex-message ex))))))

(deftest ?throw-test
  (testing "returns the message unchanged for non-errors"
    (is (= return-msg (message/?throw return-msg))))
  (testing "throws for error messages"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"D-Bus error"
         (message/?throw error-msg)))))
