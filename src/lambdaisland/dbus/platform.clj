(ns lambdaisland.dbus.platform
  "Platform-specific primitives for the D-Bus client (JVM implementation).

  This namespace is the seam between the portable D-Bus code and the JVM.
  A sibling `platform.jolt` provides the same API on Jolt (Chez Scheme)."
  (:require
   [clojure.data.xml :as xml])
  (:import
   (java.net UnixDomainSocketAddress)
   (java.nio ByteBuffer ByteOrder)
   (java.nio.channels SocketChannel)))

(set! *warn-on-reflection* true)

;;; Byte order

(defn little-endian [] :LITTLE_ENDIAN)

(defn big-endian [] :BIG_ENDIAN)

(defn- byte-order ^ByteOrder [order]
  (case order
    :LITTLE_ENDIAN ByteOrder/LITTLE_ENDIAN
    :BIG_ENDIAN ByteOrder/BIG_ENDIAN))

;;; Buffer

(defn buffer ^ByteBuffer [size]
  (ByteBuffer/allocate size))

(defn position ^long [^ByteBuffer buf]
  (.position buf))

(defn set-position! ^ByteBuffer [^ByteBuffer buf ^long n]
  (.position buf n))

(defn limit ^long [^ByteBuffer buf]
  (.limit buf))

(defn remaining ^long [^ByteBuffer buf]
  (.remaining buf))

(defn has-remaining? [^ByteBuffer buf]
  (.hasRemaining buf))

(defn flip! ^ByteBuffer [^ByteBuffer buf]
  (.flip buf))

(defn clear! ^ByteBuffer [^ByteBuffer buf]
  (.clear buf))

(defn compact! ^ByteBuffer [^ByteBuffer buf]
  (.compact buf))

(defn order [^ByteBuffer buf]
  (if (= ByteOrder/LITTLE_ENDIAN (.order buf))
    :LITTLE_ENDIAN
    :BIG_ENDIAN))

(defn set-order! ^ByteBuffer [^ByteBuffer buf order]
  (.order buf (byte-order order)))

(defn get-byte [^ByteBuffer buf]
  (.get buf))

(defn get-byte-at [^ByteBuffer buf ^long i]
  (.get buf i))

(defn get-int16 [^ByteBuffer buf]
  (.getShort buf))

(defn get-int32 [^ByteBuffer buf]
  (.getInt buf))

(defn get-int32-at [^ByteBuffer buf ^long i]
  (.getInt buf i))

(defn get-int64 [^ByteBuffer buf]
  (.getLong buf))

(defn get-double [^ByteBuffer buf]
  (.getDouble buf))

(defn get-bytes [^ByteBuffer buf ^long n]
  (let [arr (byte-array n)]
    (.get buf arr 0 n)
    arr))

(defn put-byte! ^ByteBuffer [^ByteBuffer buf v]
  (.put buf (unchecked-byte v)))

(defn put-int16! ^ByteBuffer [^ByteBuffer buf v]
  (.putShort buf (short v)))

(defn put-int32! ^ByteBuffer [^ByteBuffer buf v]
  (.putInt buf (int v)))

(defn put-int32-at! ^ByteBuffer [^ByteBuffer buf ^long i v]
  (.putInt buf i (int v)))

(defn put-int64! ^ByteBuffer [^ByteBuffer buf v]
  (.putLong buf (long v)))

(defn put-double! ^ByteBuffer [^ByteBuffer buf v]
  (.putDouble buf (double v)))

(defn put-bytes! ^ByteBuffer [^ByteBuffer buf ^bytes arr]
  (.put buf arr))

(defn copy-remaining! ^ByteBuffer [^ByteBuffer dst ^ByteBuffer src]
  (.put dst (.slice src)))

;;; Sockets

(defn open-unix-socket ^SocketChannel [^String path]
  (SocketChannel/open (UnixDomainSocketAddress/of path)))

(defn sock-read [^SocketChannel sock ^ByteBuffer buf]
  (.read sock buf))

(defn sock-write [^SocketChannel sock ^ByteBuffer buf]
  (.write sock buf))

(defn close-socket [^SocketChannel sock]
  (.close sock))

;;; Introspection XML

(defn parse-xml [s]
  (xml/parse-str s))
