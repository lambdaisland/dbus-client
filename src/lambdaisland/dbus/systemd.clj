(ns lambdaisland.dbus.systemd
  "Convenience functions for talking to systemd over the system bus."
  (:require
   [lambdaisland.dbus.client :as client]))

(defn manager-call
  "Call a method on the org.freedesktop.systemd1.Manager interface."
  [client member & args]
  (client/call client
               (into [member
                      "org.freedesktop.systemd1"
                      "/org/freedesktop/systemd1"]
                     args)))

(defn list-units [client]
  (for [[name desc load-state active-state sub-state
         followed-unit object-path job-id job-type job-object-path]
        (manager-call client 'org.freedesktop.systemd1.Manager/ListUnits)]
    {;; The unit's canonical name (e.g., `cups-browsed.service`)
     :name            name
     ;; A short, human-readable description of the unit
     :desc            desc
     ;; The unit's load state (e.g., `loaded`, `not-found`, `masked`)
     :load-state      load-state
     ;; The high-level active state (e.g., `active`, `inactive`, `failed`)
     :active-state    active-state
     ;; The low-level, unit-type-specific sub-state (e.g., `running`, `dead`)
     :sub-state       sub-state
     ;; The name of a unit this unit is bound to or follows (empty if none)
     :followed-unit   followed-unit
     ;; The D-Bus object path for the unit itself
     :object-path     object-path
     ;; The numeric ID of a job currently pending for this unit (0 if none)
     :job-id          job-id
     ;; The type of the pending job (e.g., `start`, `stop`, empty if none)
     :job-type        job-type
     ;; The D-Bus object path for the pending job (`/` if none)
     :job-object-path job-object-path}))

(defn list-jobs [client]
  (manager-call client 'org.freedesktop.systemd1.Manager/ListJobs))

(defn list-unit-files [client]
  (manager-call client 'org.freedesktop.systemd1.Manager/ListUnitFiles))

(defn get-unit
  "Return the object path for the given unit."
  [client name]
  (manager-call client 'org.freedesktop.systemd1.Manager/GetUnit name))

(defn load-unit [client name]
  (manager-call client 'org.freedesktop.systemd1.Manager/LoadUnit name))

(defn get-unit-processes [client name]
  (manager-call client 'org.freedesktop.systemd1.Manager/GetUnitProcesses name))

(defn start-unit
  "Start the unit. `mode` is one of `replace`, `fail`, `isolate`,
  `ignore-dependencies` or `ignore-requirements`. Returns the job path."
  [client name mode]
  (manager-call client 'org.freedesktop.systemd1.Manager/StartUnit name mode))

(defn start-transient-unit
  "properties: a(sv)
   aux: a(sa(sv))"
  [client name mode properties aux]
  (manager-call client 'org.freedesktop.systemd1.Manager/StartTransientUnit
                name mode properties aux))

(defn stop-unit [client name mode]
  (manager-call client 'org.freedesktop.systemd1.Manager/StopUnit name mode))

(defn restart-unit [client name mode]
  (manager-call client 'org.freedesktop.systemd1.Manager/RestartUnit name mode))

(defn try-restart-unit [client name mode]
  (manager-call client 'org.freedesktop.systemd1.Manager/TryRestartUnit name mode))

(defn reload-unit [client name mode]
  (manager-call client 'org.freedesktop.systemd1.Manager/ReloadUnit name mode))

(defn reload-or-restart-unit [client name mode]
  (manager-call client 'org.freedesktop.systemd1.Manager/ReloadOrRestartUnit name mode))

(defn reload-or-try-restart-unit [client name mode]
  (manager-call client 'org.freedesktop.systemd1.Manager/ReloadOrTryRestartUnit name mode))

(defn kill-unit
  "Send a signal to all processes of the unit. `who` is one of `main`,
  `control` or `all`, `signal` is the signal number."
  [client name who signal]
  (manager-call client 'org.freedesktop.systemd1.Manager/KillUnit name who signal))

(defn freeze-unit [client name]
  (manager-call client 'org.freedesktop.systemd1.Manager/FreezeUnit name))

(defn thaw-unit [client name]
  (manager-call client 'org.freedesktop.systemd1.Manager/ThawUnit name))

(defn reset-failed-unit [client name]
  (manager-call client 'org.freedesktop.systemd1.Manager/ResetFailedUnit name))

(defn get-default-target [client]
  (manager-call client 'org.freedesktop.systemd1.Manager/GetDefaultTarget))

(defn set-default-target [client name force]
  (manager-call client 'org.freedesktop.systemd1.Manager/SetDefaultTarget name force))

(defn reset-failed [client]
  (manager-call client 'org.freedesktop.systemd1.Manager/ResetFailed))

(defn reload [client]
  (manager-call client 'org.freedesktop.systemd1.Manager/Reload))
