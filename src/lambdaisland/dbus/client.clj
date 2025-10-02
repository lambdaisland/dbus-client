(ns lambdaisland.dbus.client
  (:require
   [lambdaisland.dbus.format :as format])
  (:import
   (java.net UnixDomainSocketAddress)
   (java.nio ByteBuffer)
   (java.nio.channels SocketChannel)
   (java.nio.charset StandardCharsets)))

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
   :version 1
   :serial 1
   :headers
   {:path "/org/freedesktop/DBus"
    :member "Hello"
    :interface "org.freedesktop.DBus"
    :destination "org.freedesktop.DBus"}})

(defn handshake [chan]
  (write-str chan
             (str auth-external
                  data-cmd
                  negotiate-unix-fd
                  begin-cmd))
  (let [buf (format/byte-buffer)
        id (loop [[line & lines] (read-handshake-lines chan buf)]
             (if-let [[_ id] (re-find #"OK ([0-9a-f]*)\r\n" line)]
               id
               (recur lines)))]
    (format/sock-write chan format/write-message hello-call)
    {:socket chan
     :buffer buf
     :id id
     :assigned-name (ffirst (:body (format/read-message (ByteBuffer/wrap (format/sock-read chan)))))}))
