(ns lambdaisland.dbus.client
  "Pure-Clojure DBUS client library"
  (:require
   [clojure.string :as str]
   [lambdaisland.dbus.format :as format]
   [lambdaisland.dbus.message :as msg]
   [lambdaisland.dbus.platform :as platform])
  (:import
   (java.nio.charset StandardCharsets)))

(set! *warn-on-reflection* true)

(def null-byte "\u0000")
(def crlf "\r\n")

(def auth-external (str null-byte "AUTH EXTERNAL" crlf))
(def data-cmd (str "DATA" crlf))
(def negotiate-unix-fd (str "NEGOTIATE_UNIX_FD" crlf))
(def begin-cmd (str "BEGIN" crlf))

(def ok-response (str "OK" crlf))
(def agree-unix-fd (str "AGREE_UNIX_FD" crlf))

(defn write-str [sock s]
  (let [b (.getBytes ^String s)
        buf (platform/buffer (count b))]
    (platform/put-bytes! buf b)
    (platform/flip! buf)
    (loop []
      (when (platform/has-remaining? buf)
        (platform/sock-write sock buf)
        (recur)))
    nil))

(defn read-handshake-lines [sock buffer]
  (let [cr 13, lf 10, crlf-len 2]
    (when (pos? (platform/remaining buffer))
      (platform/sock-read sock buffer))
    (platform/flip! buffer)

    (loop [lines []]
      (let [limit (platform/limit buffer)
            pos (platform/position buffer)
            remaining (- limit pos)]
        (if (< remaining crlf-len)
          (do (platform/compact! buffer) lines)
          (let [match-pos (loop [i pos]
                            (if (>= (+ i crlf-len) limit)
                              -1 ; Delimiter not found in remaining data
                              (if (and (= cr (platform/get-byte-at buffer i))
                                       (= lf (platform/get-byte-at buffer (inc i))))
                                i ; Match found at index i
                                (recur (inc i)))))]
            (if (= match-pos -1)
              ;; Delimiter not found: Terminate scan.
              (do (platform/compact! buffer) lines)
              ;; Delimiter Found: Extract line and recurse
              (let [line-length (+ (- match-pos pos) crlf-len)
                    line-bytes (platform/get-bytes buffer line-length)
                    line-str (String. line-bytes StandardCharsets/UTF_8)]
                ;; Position is already updated by get-bytes
                (recur (conj lines line-str))))))))))

(def hello-call
  {:type :method-call
   :flags {}
   :headers
   {:path "/org/freedesktop/DBus"
    :member "Hello"
    :interface "org.freedesktop.DBus"
    :destination "org.freedesktop.DBus"}})

(defn sock-read [buf sock]
  (let [len (platform/sock-read sock buf)]
    (if (< 0 len)
      (let [arr (byte-array len)]
        (platform/flip! buf)
        (platform/get-bytes buf len)
        arr)
      (do
        (println "WARN: read from closed channel")
        (byte-array 0)))))

(defn write-message* [{:keys [socket buffer serial replies] :as client} msg]
  (locking buffer
    (platform/clear! buffer)
    (format/write-message buffer msg)
    (platform/flip! buffer)
    (platform/sock-write socket buffer))
  nil)

(declare introspect)

(defn path-info
  [{:keys [interfaces] :as client}
   {:keys [destination path interface member] :as headers}]
  (or (-> interfaces deref (get-in [destination path]))
      (let [pinfo (introspect client headers)]
        (swap! interfaces assoc-in [destination path] pinfo)
        pinfo)))

(defn iface-info [client {:keys [destination path interface member] :as headers}]
  (get (path-info client headers) interface))

(defn method-info [client {:keys [destination path interface member] :as headers}]
  (get-in (path-info client headers) [interface :methods member]))

(defn reply-expected? [{:keys [interfaces]} {:keys [flags type headers] :as msg}]
  (let [{:keys [destination path interface member]} headers]
    (and (= :method-call type)
         (not (:no-reply-expected flags))
         (not (get-in @interfaces
                      [destination path interface
                       :methods member
                       :annotations "org.freedesktop.DBus.Method.NoReply"])))))

(defn write-message [{:keys [socket buffer serial replies] :as client} msg]
  (let [serial (swap! serial inc)
        msg    (assoc msg :serial serial)]
    (if-not (reply-expected? client msg)
      (write-message* client msg)
      (let [reply (promise)]
        (swap! replies assoc serial reply)
        (write-message* client msg)
        reply))))

(defn- expand-method-call [m]
  (cond
    (vector? m)
    (let [[method dest path & body] m]
      {:type :method-call
       :headers
       {:path        path
        :member      (name method)
        :interface   (namespace method)
        :destination dest}
       :body body})
    (map? m)
    (let [{:keys [method dest path]} m]
      (->
       {:type :method-call
        :headers
        {:path        path
         :member      (name method)
         :interface   (namespace method)
         :destination dest}}
       (merge-with (fn [a b] (if (map? a) (merge a b) b)) (dissoc m :method :dest :path))))))

(defn method-signature [client headers]
  (apply str (map :type (:in (method-info client headers)))))

(defn- single-type? [sig]
  (let [t (format/sig->type sig)]
    (not (and (vector? t) (= :tuple (first t))))))

(defn call [client method-call]
  (let [msg  (expand-method-call method-call)
        body (:body msg)
        sig  (when (seq body)
               (or (get-in msg [:headers :signature])
                   (method-signature client (:headers msg))))
        msg  (cond-> msg
               sig (assoc-in [:headers :signature] sig)
               (and sig (= 1 (count body)) (single-type? sig))
               (assoc :body (first body)))]
    (msg/body (write-message client msg))))

(defn read-fully
  "Reads bytes from the socket into the buffer until the buffer
  is full (i.e., position reaches limit), or the channel reaches end-of-stream.
  Returns the total number of bytes read in this call."
  [channel buffer]
  (loop [total-read 0]
    (let [bytes-read (platform/sock-read channel buffer)]
      (cond
        (neg? bytes-read)
        (throw (ex-info "Connection closed while attempting to read full message body." {}))

        (not (platform/has-remaining? buffer))
        (+ total-read bytes-read)

        :else
        (recur (+ total-read bytes-read))))))

(defn ensure-full-read
  "Ensures the buffer contains at least `n` bytes starting from its current
  position. Reads more data from the channel if needed, re-allocating the buffer
  if required. Returns a buffer positioned at the same logical offset, with at
  least `n` bytes available."
  [chan buf n]
  (let [available-bytes (- (platform/limit buf) (platform/position buf))]
    (if (<= n available-bytes)
      buf
      (let [needed (- n available-bytes)
            new-buf (platform/set-order! (format/byte-buffer (max n format/*default-buffer-size*))
                                         (platform/order buf))]
        (platform/copy-remaining! new-buf buf)
        (let [tmp (format/byte-buffer needed)]
          (read-fully chan tmp)
          (platform/copy-remaining! new-buf (platform/flip! tmp)))
        (platform/flip! new-buf)
        new-buf))))

(defn- message-total-length
  "Given a buffer positioned at the start of a message (with at least 16 bytes
  available), return the total on-wire length of the message: the 8-byte-aligned
  header plus the body. Messages are not themselves padded to 8 bytes, so this
  is exactly where the next message begins."
  [buf]
  (let [start (platform/position buf)
        header-len (+ 16 (bit-and (platform/get-int32-at buf (+ start 12)) 0xffffffff))
        body-len (bit-and (platform/get-int32-at buf (+ start 4)) 0xffffffff)
        pad8 (fn [n] (mod (- 8 (mod n 8)) 8))]
    (+ header-len (pad8 header-len) body-len)))

(defn- read-message*
  "Read a single message starting at `buf`'s current position, reading more from
  `socket` as needed. Returns [msg buf] with buf positioned just past the
  message; buf may be a freshly allocated buffer if the original was too small."
  [socket buf]
  (let [buf (ensure-full-read socket buf 16)
        start (platform/position buf)
        _ (platform/set-order! buf (case (char (platform/get-byte-at buf start))
                                     \l :LITTLE_ENDIAN
                                     \B :BIG_ENDIAN))
        message-total (message-total-length buf)
        buf (ensure-full-read socket buf message-total)
        msg-start (platform/position buf)]
    (binding [format/*buffer-offset* msg-start]
      (let [{:keys [headers body-length] :as msg} (format/read-message-header buf)
            sig (:signature headers)
            body (when (and sig (pos? body-length))
                   (format/read-body buf sig))]
        (platform/set-position! buf (+ msg-start message-total))
        [(cond-> msg body (assoc :body body)) buf]))))

(defn- dispatch! [client msg]
  (let [{:keys [reply-serial]} (:headers msg)]
    (when-let [reply (get @(:replies client) reply-serial)]
      (swap! (:replies client) dissoc reply-serial)
      (deliver reply msg))
    (when-let [handler (:handler client)]
      (future (handler msg)))))

(defn- process-messages
  "Dispatch every complete message currently buffered, leaving buf positioned at
  the start of any trailing partial message. Returns buf."
  [client buf]
  (let [socket (:socket client)]
    (loop [buf buf]
      (if (>= (- (platform/limit buf) (platform/position buf)) 16)
        (let [[msg buf] (read-message* socket buf)]
          (dispatch! client msg)
          (recur buf))
        buf))))

(defn init-client! [chan & [handler]]
  (write-str chan
             (str auth-external
                  data-cmd
                  negotiate-unix-fd
                  begin-cmd))
  (let [buf             (format/byte-buffer)
        read-buf        (format/byte-buffer)
        serial          (atom 0)
        replies         (atom {})
        interfaces      (atom {})
        read-loop-error (promise)
        id              (loop [[line & lines] (read-handshake-lines chan buf)]
                          (if-let [[_ id] (re-find #"OK ([0-9a-f]*)\r\n" line)]
                            id
                            (recur lines)))
        client          {:socket          chan
                         :buffer          buf
                         :read-buf        read-buf
                         :id              id
                         :replies         replies
                         :serial          serial
                         :handler         handler
                         :read-loop-error read-loop-error
                         :interfaces      interfaces}]
    (future
      (try
        (loop [buf read-buf]
          (let [len (platform/sock-read chan buf)]
            (when (pos? len)
              (platform/flip! buf)
              (let [buf (process-messages client buf)]
                (platform/compact! buf)
                (recur buf)))))
        (catch Throwable t
          (println "ERR" t)
          (deliver read-loop-error t))
        (finally
          (println "Read loop broken"))))
    (let [hello-reply (write-message client hello-call)]
      (assoc client :assigned-name (msg/body hello-reply)))))

(defn sock-conn [sock-loc]
  (platform/open-unix-socket sock-loc))

(defn session-sock []
  (let [[_ path] (re-find #"unix:path=(.*)" (System/getenv "DBUS_SESSION_BUS_ADDRESS"))]
    (sock-conn path)))

(defn system-sock []
  (sock-conn "/run/dbus/system_bus_socket"))

(defn munge-introspection
  [{:keys [attrs content]}]
  (into
   {}
   (keep (fn [{:keys [tag attrs content]}]
           (when (= :interface tag)
             [(:name attrs)
              (update-vals
               (reduce
                (fn [acc {:keys [tag attrs content]}]
                  (if-not tag
                    acc
                    (update acc (case tag
                                  :property :properties
                                  :method :methods
                                  :signal :signals
                                  tag)
                            (fnil conj [])
                            (:name attrs)
                            (reduce
                             (fn [acc {:keys [tag attrs]}]
                               (if-not tag
                                 acc
                                 (case tag
                                   :arg
                                   (if-let [d (:direction attrs)]
                                     (update acc (keyword d) (fnil conj []) (dissoc attrs :direction))
                                     (update acc :args (fnil conj []) (dissoc attrs :direction)))
                                   :annotation
                                   (assoc-in acc [:annotations (:name attrs)] (:value attrs)))))
                             {}
                             content))))
                {}
                content)
               #(apply array-map %))])))
   content))

(defn introspect* [client destination path]
  (let [body (-> client
                 (write-message
                  {:type :method-call
                   :headers
                   {:interface   "org.freedesktop.DBus.Introspectable"
                    :member      "Introspect"
                    :destination destination
                    :path        path}})
                 msg/body)]
    ;; Remove doctype, or we get a parse error on the DTD declaration.
    (-> body
        (str/replace #"^<!DOCTYPE[^>]+>" "")
        platform/parse-xml)))

(defn introspect
  ([client {:keys [destination path]}]
   (introspect client destination path))
  ([client destination path]
   (-> client (introspect* destination path) munge-introspection)))

(defn ls [client destination path]
  (map #(str path (if (= "/" path) "" "/") (:name (:attrs %)))
       (filter #(= :node (:tag %))
               (:content (introspect* client destination path)))))
