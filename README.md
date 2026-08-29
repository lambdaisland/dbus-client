# dbus-client

<!-- badges -->
[![cljdoc badge](https://cljdoc.org/badge/com.lambdaisland/dbus-client)](https://cljdoc.org/d/com.lambdaisland/dbus-client) [![Clojars Project](https://img.shields.io/clojars/v/com.lambdaisland/dbus-client.svg)](https://clojars.org/com.lambdaisland/dbus-client)
<!-- /badges -->

Pure-clojure DBUS client

## Features

- Connect to D-Bus over unix sockets (session bus, system bus)
- Separate implementation of D-Bus (un)marshalling, with type encoding, alignment, signature handling, little/big endian
- Send messages (maps) and receive replies (promise) asynchronously with low level (`write-message`) and high-level (`call`) API
- D-Bus introspection, automatic lookup and caching of method signature
- Wrapper namespaces for built-in D-Bus and Systemd interfaces (DBus, Peer, Properties, ObjectManager, Manager)

<!-- installation -->
## Installation

To use the latest release, add the following to your `deps.edn` ([Clojure CLI](https://clojure.org/guides/deps_and_cli))

```
com.lambdaisland/dbus-client {:mvn/version "0.2.15"}
```

or add the following to your `project.clj` ([Leiningen](https://leiningen.org/))

```
[com.lambdaisland/dbus-client "0.2.15"]
```
<!-- /installation -->

## Rationale

D-Bus is a widely used Interprocess Communication (IPC) mechanism providing a
Remote Procedure Call (RPC) interface, which is widely used on Linux both on a
system level (via the system bus), and by desktop environments and applications
(via the session bus).

In Clojure it would be natural to consume something like this through its Java
bindings, but the Java D-Bus library is based on reflection of Java classes and
methods, which doesn't work with a dynamically types language like Clojure.

At the end of the day it's just data over the wire, something which Clojure is
quite good at handling, someone just needs to implement (de)serialisation of the
wire format. This library does that, as well as offering a message processing
loop, and a number of utility namespaces, to be a bit more "batteries included".

## Usage

The main namespace is `lambdaisland.dbus.client`. We'll also load the `bus`
namespace, which contains wrapper for methods provided by the message bus itself.

```clj
(require
  '[lambdaisland.dbus.bus :as bus]
  '[lambdaisland.dbus.client :as dbus])
```

To get started, connect to a message bus. `system-sock` connects to the default
location of the system bus (namely: `/run/dbus/system_bus_socket`),
`session-sock` finds the session bus via the `DBUS_SESSION_BUS_ADDRESS`
environment variable.

```clj
(def client
  (dbus/init-client!
   (client/session-sock) ;; or (client/system-sock)
   (fn [v]
     ;; response handler, mostly for signal handling,
     ;; but receives all sent to us
     (println "Got reply" v))))
```

With a `client` in hand, we can send a message. A D-Bus messages takes a
"destination" (an identifier of another peer connected to the bus), a "path",
identifying a specific service endpoint or object inside the peer, and an
interface+message name available at that endpoint.

Let's first see who's connected to the bus:

```clj
(bus/list-names client)
;;=>
["org.freedesktop.DBus"
 "org.freedesktop.Notifications"
 "org.freedesktop.network-manager-applet"
 "org.freedesktop.portal.Desktop"
 "org.freedesktop.systemd1"
 "org.pipewire.Telephony"
 "org.gtk.vfs.Daemon"
 "org.pulseaudio.Server"
 "org.kde.kdeconnect"
 "org.gnome.keyring"
 ,,,
 ]
```

Notifications sounds intesting, which paths are available there?

```clj
(dbus/ls client "org.freedesktop.Notifications" "/")
;; => ("/org")
(dbus/ls client "org.freedesktop.Notifications" "/org")
;; => ("/org/erikreider" "/org/freedesktop")
(dbus/ls client "org.freedesktop.Notifications" "/org/freedesktop")
;; => ("/org/freedesktop/Notifications")
```

And which interfaces and methods can we call there?

```clj
(dbus/path-info client {:destination "org.freedesktop.Notifications"
                        :path "/org/freedesktop/Notifications"})
;;=>
{"org.freedesktop.DBus.Properties" {,,,}
 "org.freedesktop.DBus.Introspectable" {,,,}
 "org.freedesktop.DBus.Peer" {,,,}
 "org.freedesktop.Notifications"
 {:methods
  {"GetCapabilities" {:out [{:type "as", :name "result"}]},
   "Notify"
   {:in
    [{:type "s", :name "app_name"}
     {:type "u", :name "replaces_id"}
     {:type "s", :name "app_icon"}
     {:type "s", :name "summary"}
     {:type "s", :name "body"}
     {:type "as", :name "actions"}
     {:type "a{sv}", :name "hints"}
     {:type "i", :name "expire_timeout"}],
    :out [{:type "u", :name "result"}]},
   "CloseNotification" {:in [{:type "u", :name "id"}]},
   "GetServerInformation"
   {:out
    [{:type "s", :name "name"}
     {:type "s", :name "vendor"}
     {:type "s", :name "version"}
     {:type "s", :name "spec_version"}]}},
  :signals
  {"NotificationClosed"
   {:args [{:type "u", :name "id"} {:type "u", :name "reason"}]},
   "ActionInvoked"
   {:args [{:type "u", :name "id"} {:type "s", :name "action_key"}]},
   "ActivationToken"
   {:args [{:type "u", :name "id"} {:type "s", :name "activation_token"}]},
   "NotificationReplied"
   {:args [{:type "u", :name "id"} {:type "s", :name "text"}]}}}}
```

Seems it implements a number of common standard interfaces (Properties,
Introspectable, Peer), but it also has a `org.freedesktop.Notifications`
interface, with methods like `Notify` and `CloseNotification`. Let's try to
create a desktop notification!

```clj
(dbus/call client
           ['org.freedesktop.Notifications/Notify
            "org.freedesktop.Notifications"
            "/org/freedesktop/Notifications"
            "App name"
            0              ; replaces_id
            ""             ; app_icon
            "Cool summary"
            "Hello from Clojure!"
            []             ; actions
            []             ; hints
            10000])        ; timeout
;; => 127
```

That worked!

The vector passed to `call` is conceived similar to a method call, the first
element is a namespaced symbol (keyword works too) of `interface/method`,
followed by destination, path, and any body arguments.

`call` will block until a response arrives. Instead of a vector, it will also
accept a map:

```clj
{:method 'interface/method
 :dest "my.dest"
 :path "/my/path"
 :body [body args]}
```

### Signals

To receive signals, subscribe to them with `bus/add-match`

```clj
(bus/add-match client
               {:type      "signal"
                :sender    "org.freedesktop.Notifications"
                :interface "org.freedesktop.Notifications"
                :member    "NotificationClosed"
                :path      "/org/freedesktop/Notifications"})
```

Now when the user closes a notification, the callback passed to `init-client!`
will be called with the following:

```clj
{:endian :LITTLE_ENDIAN,
 :type :signal,
 :flags {:no-reply-expected true},
 :version 1,
 :body-length 8,
 :serial 920404,
 :headers
 {:path "/org/freedesktop/Notifications",
  :interface "org.freedesktop.Notifications",
  :signature "uu",
  :member "NotificationClosed",
  :sender ":1.54"},
 :body [134 2]}
```

Which is the raw message map that we got from the message bus.

From the introspection result earlier we know how to interpret the body, namely as `[id reason]`

```clj
{:signals {"NotificationClosed" {:args [{:type "u", :name "id"} {:type "u", :name "reason"}]}}
```

<!-- opencollective -->
## Lambda Island Open Source

Thank you! dbus-client is made possible thanks to our generous backers. [Become a
backer on OpenCollective](https://opencollective.com/lambda-island) so that we
can continue to make dbus-client better.

<a href="https://opencollective.com/lambda-island">
<img src="https://opencollective.com/lambda-island/organizations.svg?avatarHeight=46&width=800&button=false">
<img src="https://opencollective.com/lambda-island/individuals.svg?avatarHeight=46&width=800&button=false">
</a>
<img align="left" src="https://github.com/lambdaisland/open-source/raw/master/artwork/lighthouse_readme.png">

&nbsp;

dbus-client is part of a growing collection of quality Clojure libraries created and maintained
by the fine folks at [Gaiwan](https://gaiwan.co).

Pay it forward by [becoming a backer on our OpenCollective](http://opencollective.com/lambda-island),
so that we continue to enjoy a thriving Clojure ecosystem.

You can find an overview of all our different projects at [lambdaisland/open-source](https://github.com/lambdaisland/open-source).

&nbsp;

&nbsp;
<!-- /opencollective -->

<!-- contributing -->
## Contributing

We warmly welcome patches to dbus-client. Please keep in mind the following:

- adhere to the [LambdaIsland Clojure Style Guide](https://nextjournal.com/lambdaisland/clojure-style-guide)
- write patches that solve a problem
- start by stating the problem, then supply a minimal solution `*`
- by contributing you agree to license your contributions as MPL 2.0
- don't break the contract with downstream consumers `**`
- don't break the tests

We would very much appreciate it if you also

- update the CHANGELOG and README
- add tests for new functionality

We recommend opening an issue first, before opening a pull request. That way we
can make sure we agree what the problem is, and discuss how best to solve it.
This is especially true if you add new dependencies, or significantly increase
the API surface. In cases like these we need to decide if these changes are in
line with the project's goals.

`*` This goes for features too, a feature needs to solve a problem. State the problem it solves first, only then move on to solving it.

`**` Projects that have a version that starts with `0.` may still see breaking changes, although we also consider the level of community adoption. The more widespread a project is, the less likely we're willing to introduce breakage. See [LambdaIsland-flavored Versioning](https://github.com/lambdaisland/open-source#lambdaisland-flavored-versioning) for more info.
<!-- /contributing -->

<!-- license -->
## License

Copyright &copy; 2025-2026 Arne Brasseur and Contributors

Licensed under the term of the Mozilla Public License 2.0, see LICENSE.
<!-- /license -->
