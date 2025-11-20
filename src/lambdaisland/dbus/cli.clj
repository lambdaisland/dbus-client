#!/usr/bin/env bb
(ns lambdaisland.dbus.cli
  (:require
   [lambdaisland.dbus.client :as client]
   [clojure.pprint :as pprint]
   [lambdaisland.cli :as cli]))

(def init {:queue (java.util.concurrent.LinkedBlockingQueue.)})

(defn list-names [opts]
  (run! println
        (:body
         @(client/write-message
           (:client opts)
           {:type :method-call
            :headers
            {:path "/org/freedesktop/DBus"
             :member "ListNames"
             :interface "org.freedesktop.DBus"
             :destination "org.freedesktop.DBus"}}))))

(defn introspect
  [opts]
  (pprint/pprint
   (client/introspect (:client opts) {:destination (:dest opts) :path (:path opts)})))

(defn listen [opts]
  @(client/write-message
    (:client opts)
    {:type :method-call
     :headers
     {:path "/org/freedesktop/DBus"
      :member "AddMatch"
      :signature "s"
      :interface "org.freedesktop.DBus"
      :destination "org.freedesktop.DBus"}
     :body (:match-rule opts)})
  (.forEach (:queue opts) #(println ">" (pr-str %))))

(defn wrap-client [f]
  (fn [opts]
    (f
     (assoc
       opts
       :client
       (client/init-client!
        (if (:session opts)
          (client/session-sock)
          (client/system-sock))
        #(.put ^java.util.concurrent.BlockingQueue (:queue opts) %)
        #_#(println "> " (pr-str %)))))))

(cli/dispatch*
 {:name "lambdaisland-dbus-cli"
  :init init
  :flags ["--session" "Use session bus"]
  :commands ["list-names" #'list-names
             "introspect <dest> <path>" #'introspect
             "listen <match-rule>" #'listen]
  :middleware [wrap-client]}
 )
