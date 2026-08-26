(ns lambdaisland.dbus.std
  "Helpers for the standard D-Bus interfaces: Peer, Properties and ObjectManager."
  (:require
   [lambdaisland.dbus.client :as client]))

(defn ping [client dest path]
  (client/call client ['org.freedesktop.DBus.Peer/Ping dest path]))

(defn machine-id [client dest path]
  (client/call client ['org.freedesktop.DBus.Peer/GetMachineId dest path]))

(defn get-property [client dest path iface prop]
  (client/call client ['org.freedesktop.DBus.Properties/Get dest path iface prop]))

(defn set-property [client dest path iface prop value]
  (client/call client ['org.freedesktop.DBus.Properties/Set dest path iface prop value]))

(defn properties [client dest path iface]
  (client/call client ['org.freedesktop.DBus.Properties/GetAll dest path iface]))

(defn managed-objects [client dest path]
  (client/call client ['org.freedesktop.DBus.ObjectManager/GetManagedObjects dest path]))
