(ns lambdaisland.dbus.systemd
  (:require
   [lambdaisland.dbus.client :as client]))

(defn list-units [client]
  (let [{:keys [body]} @(client/write-message
                         client
                         {:type :method-call
                          :headers
                          {:destination "org.freedesktop.systemd1"
                           :path "/org/freedesktop/systemd1"
                           :interface "org.freedesktop.systemd1.Manager"
                           :member "ListUnits"}})]
    (for [[name desc load-state active-state sub-state
           followed-unit object-path job-id job-type job-object-path] body]
      {;; The unit's canonical name (e.g., `cups-browsed.service`)
       :name name
       ;; A short, human-readable description of the unit
       :desc desc
       ;; The unit's load state (e.g., `loaded`, `not-found`, `masked`)
       :load-state load-state
       ;; The high-level active state (e.g., `active`, `inactive`, `failed`)
       :active-state active-state
       ;; The low-level, unit-type-specific sub-state (e.g., `running`, `dead`)
       :sub-state sub-state
       ;; The name of a unit this unit is bound to or follows (empty if none)
       :followed-unit followed-unit
       ;; The D-Bus object path for the unit itself
       :object-path object-path
       ;; The numeric ID of a job currently pending for this unit (0 if none)
       :job-id job-id
       ;; The type of the pending job (e.g., `start`, `stop`, empty if none)
       :job-type job-type
       ;; The D-Bus object path for the pending job (`/` if none)
       :job-object-path job-object-path})))
