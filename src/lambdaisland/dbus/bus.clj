(ns lambdaisland.dbus.bus
  "Convenience functions for the message bus interface (org.freedesktop.DBus)."
  (:require
   [lambdaisland.dbus.client :as client]
   [clojure.string :as str]))

(defn dbus-call
  "Call a method on the org.freedesktop.DBus interface."
  [client member & args]
  (client/call client
               (into [member
                      "org.freedesktop.DBus"
                      "/org/freedesktop/DBus"]
                     args)))

(defn list-names [client]
  (dbus-call client 'org.freedesktop.DBus/ListNames))

(defn list-activatable-names [client]
  (dbus-call client 'org.freedesktop.DBus/ListActivatableNames))

(defn name-has-owner [client name]
  (dbus-call client 'org.freedesktop.DBus/NameHasOwner name))

(defn get-name-owner [client name]
  (dbus-call client 'org.freedesktop.DBus/GetNameOwner name))

(defn request-name [client name & [flags]]
  (dbus-call client 'org.freedesktop.DBus/RequestName name (or flags 0)))

(defn release-name [client name]
  (dbus-call client 'org.freedesktop.DBus/ReleaseName name))

(defn start-service-by-name [client name & [flags]]
  (dbus-call client 'org.freedesktop.DBus/StartServiceByName name (or flags 0)))

(defn get-connection-unix-user [client name]
  (dbus-call client 'org.freedesktop.DBus/GetConnectionUnixUser name))

(defn get-connection-unix-process-id [client name]
  (dbus-call client 'org.freedesktop.DBus/GetConnectionUnixProcessID name))

(defn add-match [client rule]
  (dbus-call client 'org.freedesktop.DBus/AddMatch
             (if (map? rule)
               (str/join "," (map (fn [[k v]]
                                    (str (name k) "='" v "'"))
                                  rule))
               rule)))

(defn remove-match [client rule]
  (dbus-call client 'org.freedesktop.DBus/RemoveMatch rule))

(defn get-id [client]
  (dbus-call client 'org.freedesktop.DBus/GetId))
