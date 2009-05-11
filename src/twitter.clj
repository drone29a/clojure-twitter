(ns twitter
  (:use [clojure.http.client]
        [clojure.contrib.json.read])
  (:require [clojure.http.resourcefully :as resourcefully]
            [clojure.contrib.str-utils :as str-utils]))

(def *twitter-url* "http://twitter.com")

(defn- twitter-request
  "Appends the rest-uri to the twitter-url, makes the request and returns
the result of parsing the first part of the body.  It is assumed that JSON
is being used."
  ([rest-uri]
     (-> (request (url (str *twitter-url* rest-uri)))
         :body-seq
         first
         read-json-string))
  ([type rest-uri]
     (condp = type
       :search (binding [*twitter-url* "http://search.twitter.com"]
                 (twitter-request rest-uri))
       (twitter-request rest-uri))))

(defn- twitter-request*
  "Appends the rest-uri to the twitter-url, makes the request and returns
the result of parsing the first part of the body.  It is assumed that JSON
is being used."
  [rest-uri]
  (request (url (str twitter-url rest-uri))))

(defn id
  "ID of a user given screen name."
  [name]
  (-> (twitter-request (str "/users/show.json?screen_name=" name))
      (get "id")))

(defn friends
  "IDs of a user's friends."
  [id]
  (twitter-request (str "/friends/ids.json?user_id=" id)))

(defn followers 
  "IDs of a user's followers."
  [id]
  (twitter-request (str "/followers/ids.json?user_id=" id)))

(defn search
  "Run a search query."
  ([query]
     (search query 100 1))
  ([query rpp]
     (search query rpp 1))
  ([query rpp page]
     (twitter-request :search (str "/search.json?q=" query "&rpp=" rpp "&page=" page))))

(defn query-all
  "Build up a search string where all terms are OR'd."
  [& terms]
  (url-encode 
   (str-utils/str-join " OR " 
                       (map #(str "\"" % "\"") terms))))