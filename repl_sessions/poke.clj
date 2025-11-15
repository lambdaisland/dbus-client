(ns repl-sessions.poke
  (:require
   [lambdaisland.dbus.client :as client]
   [lambdaisland.dbus.systemd :as systemd]))

(def client (client/init-client! (client/system-sock) (fn [v]
                                                        (prn '-> v))))

(let [client (client/init-client! (client/session-sock)
                                  (fn [v]
                                    (prn '-> v)))
      buf (format/byte-buffer)]
  (client/sock-read (:buffer )))

(def client (client/init-client! (client/session-sock)
                                 (fn [v]
                                   (prn '-> v))))

client

(client/write-message client client/hello-call)
@(client/write-message
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
  @(client/write-message
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


(client/introspect client {:destination "org.freedesktop.secrets"
                           :path "/org/freedesktop/secrets"})

(def client (client/init-client! (client/system-sock)))

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
