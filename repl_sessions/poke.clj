(ns repl-sessions.poke
  (:require
   [lambdaisland.dbus.bus :as bus]
   [lambdaisland.dbus.client :as dbus]
   [lambdaisland.dbus.systemd :as systemd]))

(def client (dbus/init-client! (dbus/session-sock) #_(dbus/system-sock) (fn [v]
                                                                          (prn '-> v))))


(bus/list-names client)
(dbus/ls client "org.freedesktop.Notifications" "/")
;; => ("/org")
(dbus/ls client "org.freedesktop.Notifications" "/org")
;; => ("/org/erikreider" "/org/freedesktop")
(dbus/ls client "org.freedesktop.Notifications" "/org/freedesktop")
;; => ("/org/freedesktop/Notifications")

(dbus/ls client "org.freedesktop.Notifications" "/org/erikreider")
;; => ("/org/erikreider/swaync")
(dbus/ls client "org.freedesktop.Notifications" "/org/erikreider/swaync/window/1")
(dbus/ls client "org.freedesktop.Notifications" "/org/erikreider/swaync/cc")
;; => ("/org/erikreider/swaync/window" "/org/erikreider/swaync/cc")

(dbus/path-info client {:destination "org.freedesktop.Notifications"
                        :path "/org/freedesktop/Notifications"})

(bus/add-match client
               {:type      "signal"
                :sender    "org.freedesktop.Notifications"
                :interface "org.freedesktop.Notifications"
                :member    "NotificationClosed"
                :path      "/org/freedesktop/Notifications"})
(dbus/call client
           [:org.freedesktop.Notifications/Notify
            "org.freedesktop.Notifications"
            "/org/freedesktop/Notifications"
            "App name"
            0
            ""
            "Cool summary"
            "Hello from Clojure!"
            []
            []
            10000])
;; => 127

(dbus/call client
           ['org.freedesktop.Notifications/Notify
            "org.freedesktop.Notifications"
            "/org/freedesktop/Notifications"
            "App name"
            126
            ""
            "Cool summary"
            "Hello from Clojure again!"
            []
            []
            10000
            ])

(let [client (dbus/init-client! (dbus/session-sock)
                                (fn [v]
                                  (prn '-> v)))
      buf (format/byte-buffer)]
  (dbus/sock-read (:buffer )))

(def client (dbus/init-client! (dbus/session-sock)
                                 (fn [v]
                                   (prn '-> v))))

client

(dbus/write-message client dbus/hello-call)
@(dbus/write-message
  client
  {:type :method-call
   :headers
   {
    :path "/org/freedesktop/DBus"
    :destination "org.freedesktop.secrets"
    :member "GetManagedObjects"
    :interface "org.freedesktop.DBus.ObjectManager"}}
  #_{:type :method-call
     :headers
     {:path "/org/freedesktop/DBus"
      :member "ListNames"
      :interface "org.freedesktop.DBus"
      :destination "org.freedesktop.DBus"}}
  #_
  {:type :method-call
   :headers
   {:member "Introspect"
    :path "/org/freedesktop/DBus"
    :interface "org.freedesktop.DBus.Introspectable"}}
  #_

  {:type :method-call
   :headers
   {:member "Ping"
    :path "/org/freedesktop/DBus"
    :interface "org.freedesktop.DBus.Peer"}})

(defn poke! [msg]
  @(dbus/write-message
    client
    msg))

(poke!
 {:type :method-call
  :headers
  {:path "/org/freedesktop/DBus"
   :member "ListNames"
   :interface "org.freedesktop.DBus"
   :destination "org.freedesktop.DBus"}})

(poke!
 {:type :method-call
  :headers
  {:member "Introspect"
   :destination	"org.freedesktop.secrets"
   :path "/org/freedesktop/secrets"
   :interface "org.freedesktop.DBus.Introspectable"}})


(dbus/introspect client "org.freedesktop.systemd1" "/org/freedesktop/LogControl1")

(dbus/call client
           ['org.freedesktop.DBus.Properties/Get
            "org.freedesktop.systemd1" "/org/freedesktop/LogControl1"
            "org.freedesktop.LogControl1" "LogTarget"])

(dbus/method-sig client
                 {:interface "org.freedesktop.DBus.Properties"
                  :member "Get"
                  :destination "org.freedesktop.systemd1"
                  :path "/"})

(time
 (dbus/fetch-signature client
                       {:interface "org.freedesktop.DBus.Properties"
                        :member "Get"
                        :destination "org.freedesktop.systemd1"
                        :path "/"}))

(get-in
 @(:interfaces client)
 ["org.freedesktop.systemd1" "/" "org.freedesktop.DBus.Properties"  ])
(:body
 @(dbus/write-message client
                      {:type :method-call
                       :headers
                       {:interface "org.freedesktop.DBus.ObjectManager"
                        :member "GetManagedObjects"
                        :destination "org.freedesktop.systemd1"
                        :path "/"}}))

(:body
 @(dbus/write-message client
                      {:type :method-call
                       :headers
                       {:interface   "org.freedesktop.DBus.Introspectable"
                        :member      "Introspect"
                        :destination "org.freedesktop.systemd1"
                        :path "/org/freedesktop/systemd1"}}))

(def client (dbus/init-client! (dbus/system-sock)))

(doseq [p (map :object-path (systemd/list-units client))]
  (println "----" p "-----")
  (clojure.pprint/pprint
   (poke!
    {:type :method-call
     :headers
     {:path  p
      :member "GetAll"
      :interface "org.freedesktop.DBus.Properties"
      :destination "org.freedesktop.systemd1"
      :signature "s"}
     :body
     "org.freedesktop.systemd1.Unit"})))

(poke!
 {:type :method-call
  :headers
  {:path "/org/freedesktop/DBus"
   :member "AddMatch"
   :signature "s"
   :interface "org.freedesktop.DBus"
   :destination "org.freedesktop.DBus"}
  :body
  "type='signal',path='/org/freedesktop/systemd1/unit/forgejo_2eservice'"
  #_"type='signal',sender='org.freedesktop.systemd1',interface='org.freedesktop.DBus.Properties',member='PropertiesChanged',path='/org/freedesktop/systemd1/unit/forgejo_2eservice'"})
;; => {:endian :LITTLE_ENDIAN,
;;     :type :method-return,
;;     :flags {:no-reply-expected true},
;;     :version 1,
;;     :body-length 0,
;;     :serial 4,
;;     :headers
;;     {:destination ":1.388124", :reply-serial 3, :sender "org.freedesktop.DBus"}}


{:interface   ""
 :member      ""
 :destination
 :path }

;; Vector form
[org.freedesktop.DBus.Introspectable/Introspect
 "org.freedesktop.systemd1"
 "/org/freedesktop/systemd1"
 1 2 3
 ]

;; Map form
{:method 'org.freedesktop.DBus.Introspectable/Introspect
 :dest "org.freedesktop.systemd1"
 :path "/org/freedesktop/systemd1"
 :body [1 2 3]}
