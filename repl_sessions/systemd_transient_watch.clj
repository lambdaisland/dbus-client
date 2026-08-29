(ns repl-sessions.systemd-transient-watch
  (:require
   [lambdaisland.dbus.bus :as bus]
   [lambdaisland.dbus.client :as client]
   [lambdaisland.dbus.std :as std]
   [lambdaisland.dbus.systemd :as systemd]))

#_(when (and @auto-recover? (= "failed" result))
    (println "  !! unit failed — auto-recovering (reset-failed + re-create)")
    (systemd/reset-failed-unit @client-ref unit)
    (start-unit! healthy-props))

(def client
  (client/init-client! (client/session-sock)
                       (fn [msg]
                         (when (= :signal (:type msg))
                           (locking *out*
                             (println "=>" (-> msg :headers :member) (:body msg)))))))

(bus/add-match client
               {:type      "signal"
                :sender    "org.freedesktop.systemd1"
                :interface "org.freedesktop.systemd1.Manager"
                ;; :member    "JobNew"
                :path      "/org/freedesktop/systemd1"})
(bus/add-match client
               {:type      "signal"
                :sender    "org.freedesktop.systemd1"
                :interface "org.freedesktop.systemd1.Manager"
                :member    "JobRemoved"
                :path      "/org/freedesktop/systemd1"})


;; `a(sv)` transient-unit properties. ExecStart is `a(sasb)`: an array of
;; (path, argv, ignore-failure) structs. Lists are D-Bus arrays, vectors are
;; structs, so this derives the correct signature.
(defn exec-start [argv]
  (list [(first argv) (apply list argv) false]))

(defn service-props [argv]
  [["Description" "dbus-client transient PoC"]
   ["Type" "exec"]
   ["ExecStart" (exec-start argv)]])

(def healthy-props (service-props ["/bin/sleep" "100000"]))

(def failing-props
  [["Description" "dbus-client transient PoC (failing)"]
   ["Type" "oneshot"]
   ["ExecStart" '(["/bin/false" ("/bin/false") false])]])

(systemd/start-transient-unit client "dbus-poc.service" "replace" healthy-props [])
(systemd/restart-unit client "dbus-poc.service" "replace")
(systemd/stop-unit client "dbus-poc.service" "replace")
