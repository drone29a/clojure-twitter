(ns clojure.http.client
  (:use [clojure.contrib.java-utils :only [as-str]]
        [clojure.contrib.duck-streams :only [read-lines spit]]
        [clojure.contrib.str-utils :only [str-join]])
  (:import (java.net URL URLEncoder)
           (java.io StringReader InputStream)))

(def default-headers {"User-Agent" (str "Clojure/" (clojure-version)
                                        " (+http://clojure.org)"),
                      "Connection" "close"})

(defn url-encode
  "Wrapper around java.net.URLEncoder returning a (UTF-8) URL encoded
representation of text."
  [text]
  (URLEncoder/encode text "UTF-8"))

(defn- encode-body-map
  "Turns a map into a URL-encoded string suitable for a request body."
  [body]
  (str-join "&" (map #(str-join "=" (map url-encode %)) body)))

(defn- send-body
  [body connection headers]
  (.setDoOutput connection true)
  ;; this isn't perfect, since it doesn't account for
  ;; different capitalization etc
  (when (and (map? body)
             (not (contains? headers "Content-Type")))
    (.setRequestProperty connection
                         "Content-Type"
                         "application/x-www-form-urlencoded"))

  (.connect connection)

  (let [out (.getOutputStream connection)]
    (cond
      (string? body) (spit out body)
      (map? body) (spit out (encode-body-map body))
      (instance? InputStream body) (let [bytes (make-array Byte/TYPE 1000)]
                                     (loop [bytes-read (.read body bytes)]
                                       (when (pos? bytes-read)
                                         (.write out bytes 0 bytes-read)
                                         (recur (.read body bytes))))))
    (.close out)))

(defn url
  "If u is an instance of java.net.URL then returns it without
modification, otherwise tries to instantiate a java.net.URL with
url as its sole argument."
  [u]
  (if (instance? URL u)
    u
    (URL. u)))

(defn- body-seq
  "Returns a lazy-seq of lines from either the input stream
or the error stream of connection, whichever is appropriate."
  [connection]
  (read-lines (or (if (>= (.getResponseCode connection) 400)
                    (.getErrorStream connection)
                    (.getInputStream connection))
                  (StringReader. ""))))

(defn- parse-headers
  "Returns a map of the response headers from connection."
  [connection]
  (let [hs (.getHeaderFields connection)]
    (apply merge (map (fn [e] (when-let [k (key e)]
                                {k (first (val e))}))
                      hs))))

(defn- parse-cookies
  "Returns a map of cookies when given the Set-Cookie string sent
by a server."
  [cookie-string]
  (when cookie-string
    (apply merge
           (map (fn [cookie]
                  (apply hash-map
                         (map (fn [c]
                                (.trim c))
                              (.split cookie "="))))
                (.split cookie-string ";")))))

(defn- create-cookie-string
  "Returns a string suitable for sending to the server in the
\"Cookie\" header when given a clojure map of cookies."
  [cookie-map]
  (str-join "; " (map (fn [cookie]
                        (str (as-str (key cookie))
                             "="
                             (as-str (val cookie))))
                      cookie-map)))

(defn request
  "Perform an HTTP request on url u. "
  [u & [method headers cookies body]]
  (let [connection (.openConnection (url u))
        method (.toUpperCase (as-str (or method
                                         "GET")))]
    (.setRequestMethod connection method)

    (doseq [header (conj default-headers (or headers {}))]
      (.setRequestProperty connection
                           (first header)
                           (second header)))

    (when (and cookies (not (empty? cookies)))
      (.setRequestProperty connection
                           "Cookie"
                           (create-cookie-string cookies)))
    (if body
      (send-body body connection headers)
      (.connect connection))

    (let [headers (parse-headers connection)]
      {:body-seq (body-seq connection)
       :code (.getResponseCode connection)
       :msg (.getResponseMessage connection)
       :method method
       :headers (dissoc headers "Set-Cookie")
       ;; This correctly implements case-insensitive lookup.
       :get-header #(.getHeaderField connection (as-str %))
       :cookies (parse-cookies (headers "Set-Cookie"))
       :url (str (.getURL connection))})))
