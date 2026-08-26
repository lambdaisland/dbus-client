(ns lambdaisland.dbus.message
  "Helpers over dbus message maps")

(defn error? [msg]
  (= :error (:type msg)))

(defn error-name [msg]
  (get-in msg [:headers :error-name]))

(defn error-message [msg]
  (first (:body msg)))

(defn ?ex [msg]
  (when (error? msg)
    (ex-info (str "D-Bus error: " (error-name msg))
             {:type :lambdaisland.dbus/error
              :error-name (error-name msg)
              :error-message (error-message msg)
              :message msg})))

(defn ?throw [msg]
  (if-let [ex (?ex msg)]
    (throw ex)
    msg))

(defn body
  ([reply]
   (when reply
     (:body (?throw @reply))))
  ([reply timeout]
   (when reply
     (:body (?throw (deref reply timeout :lambdaisland.dbus/timeout))))))
