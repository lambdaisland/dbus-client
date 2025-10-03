(ns lambdaisland.dbus.client
  (:require
   [clojure.string :as str]
   [lambdaisland.dbus.format :as format])
  (:import
   (java.net UnixDomainSocketAddress)
   (java.nio ByteBuffer)
   (java.nio.channels SocketChannel)
   (java.nio.charset StandardCharsets)
   (java.util.concurrent.atomic AtomicInteger)))

(set! *warn-on-reflection* true)

(def null-byte "\u0000")
(def crlf "\r\n")

(def auth-external (str null-byte "AUTH EXTERNAL" crlf))
(def data-cmd (str "DATA" crlf))
(def negotiate-unix-fd (str "NEGOTIATE_UNIX_FD" crlf))
(def begin-cmd (str "BEGIN" crlf))

(def ok-response (str "OK" crlf))
(def agree-unix-fd (str "AGREE_UNIX_FD" crlf))

(defn write-str [^SocketChannel chan ^String s]
  (let [^bytes b (.getBytes s)]
    (loop [offset 0]
      (when (< offset (count b))
        (let [buf (ByteBuffer/wrap b offset (- (count b) offset))
              written (.write chan buf)]
          (recur (+ offset written)))))))

(defn read-handshake-lines [^SocketChannel chan ^ByteBuffer buffer]
  (let [cr 13, lf 10, crlf-len 2]
    (when (pos? (.remaining buffer))
      (.read chan buffer))
    (.flip buffer)

    (loop [lines []]
      (let [limit (.limit buffer)
            pos (.position buffer)
            remaining (- limit pos)]
        (if (< remaining crlf-len)
          (do (.compact buffer) lines)
          (let [match-pos (loop [i pos]
                            (if (>= (+ i crlf-len) limit)
                              -1 ; Delimiter not found in remaining data
                              (if (and (= cr (.get buffer i))
                                       (= lf (.get buffer (inc i))))
                                i ; Match found at index i
                                (recur (inc i)))))]

            (if (= match-pos -1)
              ;; Delimiter not found: Terminate scan.
              (do (.compact buffer) lines)

              ;; Delimiter Found: Extract line and recurse
              (let [line-length (+ (- match-pos pos) crlf-len)
                    line-bytes (byte-array line-length)
                    _ (.get buffer line-bytes 0 line-length) ; Reads from current position (pos) & moves position
                    line-str (String. line-bytes StandardCharsets/UTF_8)]

                ;; Position is already updated by the .get call!
                (recur (conj lines line-str))))))))))

(def hello-call
  {:type :method-call
   :flags {}
   :headers
   {:path "/org/freedesktop/DBus"
    :member "Hello"
    :interface "org.freedesktop.DBus"
    :destination "org.freedesktop.DBus"}})

(defn sock-read ^bytes [^ByteBuffer buf ^SocketChannel chan]
  (.mark buf)
  (let [len (.read chan buf)]
    (if (< 0 len)
      (let [arr (byte-array len)]
        (.flip buf)
        (.get buf arr 0 len)
        arr)
      (do
        (println "WARN: read from closed channel")
        (byte-array 0)))))

(defn write-message [{:keys [^SocketChannel socket ^ByteBuffer buffer serial replies]} msg]
  (let [serial (.incrementAndGet ^AtomicInteger serial)
        reply (promise)]
    (println "SERIAL" serial)
    (swap! replies assoc serial reply)
    (.clear buffer)
    (format/write-message buffer (doto (assoc msg :serial serial) prn))
    (.flip buffer)
    (.write socket buffer)
    reply))

(defn init-client! [^SocketChannel chan handler]
  (write-str chan
             (str auth-external
                  data-cmd
                  negotiate-unix-fd
                  begin-cmd))
  (let [buf (format/byte-buffer)
        read-buf (format/byte-buffer)
        serial (AtomicInteger. 0)
        replies (atom {})
        id (loop [[line & lines] (read-handshake-lines chan buf)]
             (println line)
             (if-let [[_ id] (re-find #"OK ([0-9a-f]*)\r\n" line)]
               id
               (recur lines)))
        client {:socket chan
                :buffer buf
                :id id
                :replies replies
                :serial serial
                :handler handler}]
    (future
      (try
        (while true
          (.clear read-buf)
          (println "READ" (.read chan read-buf))
          (.flip read-buf)
          (let [msg (format/read-message read-buf)]
            (when-let [reply (get @replies (get-in msg [:headers :reply-serial]))]
              (swap! replies dissoc (:serial msg))
              (deliver reply msg))
            (handler msg)))
        (catch Throwable t
          (def ttt t))
        (finally
          (println "Read loop broken"))))
    (let [hello-reply (write-message client hello-call)]
      (assoc client :assigned-name (:body @hello-reply)))))

(defn sock-conn ^SocketChannel [^String sock-loc]
  (SocketChannel/open (UnixDomainSocketAddress/of sock-loc)))

(defn session-sock []
  (let [[_ path] (re-find #"unix:path=(.*)" (System/getenv "DBUS_SESSION_BUS_ADDRESS"))]
    (sock-conn path)))

(defn system-sock []
  (sock-conn "/run/dbus/system_bus_socket"))

(defn munge-introspection [{:keys [attrs content]}]
  (reduce (fn [acc o]
            (assoc-in acc [({:property :properties}
                            (:tag o)
                            (keyword (str (name (:tag o)) "s")))
                           (get-in o [:attrs :name])]
                      (munge-introspection o)))
          attrs
          (filter map? content)))

(defn introspect [client {:keys [destination path]}]
  (let [body (:body
              @(write-message client
                              {:type :method-call
                               :headers
                               {:interface   "org.freedesktop.DBus.Introspectable"
                                :member      "Introspect"
                                :destination destination
                                :path        path}}))]
    ;; Remove doctype, or we get
    ;; 1. Unhandled javax.xml.stream.XMLStreamException
    ;; ParseError at [row,col]:[1,3] Message: The markup declarations contained or
    ;; pointed to by the document type declaration must be well-formed.
    ;; XMLStreamReaderImpl.java:  652  com.sun.org.apache.xerces.internal.impl.XMLStreamReaderImpl/next
    (-> body
        (str/replace #"^<!DOCTYPE[^>]+>" "")
        xml/parse-str
        munge-introspection)))
