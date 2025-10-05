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

(def client  (client/init-client! (client/session-sock)
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

(def client (client/init-client! (client/system-sock) (fn [v]
                                                        (prn '-> v))))

@(client/write-message
  client
  {:type :method-call
   :headers
   {:destination "org.freedesktop.systemd1"
    :path "/org/freedesktop/systemd1"
    :interface "org.freedesktop.systemd1.Manager"
    :member "ListUnits"}})

(systemd/list-units client)
