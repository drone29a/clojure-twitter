(ns twitter
  (:refer-clojure :exclude [name])
  (:use [clojure.http.client]
        [clojure.contrib.json.read])
  (:require [clojure.http.resourcefully :as resourcefully]
            [clojure.contrib.str-utils :as str-utils]
            [twitter.query :as query]))

(def rate-limit-reached (atom nil))

(def *twitter-uri* nil)

(def *settings* {:base-uri "http://twitter.com"
                 :search-uri "http://search.twitter.com"
                 :basic-auth nil})

(defn- twitter-request
  "Appends the rest-uri to the twitter-uri, makes the request and returns
the result of parsing the first part of the body.  It is assumed that JSON
is being used."
  ([rest-uri]
     (if-not @rate-limit-reached
       (binding [*twitter-uri* (or *twitter-uri* (*settings* :base-uri))]
         (let [result (request (url (str *twitter-uri* rest-uri)) 
                               :get 
                               (when-let [auth (*settings* :basic-auth)]
                                 {"Authorization" (str "Basic " auth)}))]
           (condp #(if (coll? %1)  
                     (first (filter (fn [x] (== x %2)) %1))
                     (== %2 %1)) (:code result)
             200 (-> result
                     :body-seq
                     first
                     read-json-string)
             304 nil
             [400 401 403 404 406 500 502 503] (let [body (-> (first (:body-seq result))
                                                              read-json-string)
                                                     headers (:headers result)
                                                     error-msg (body "error")
                                                     request-uri (body "request")]
                                                 (when (= (headers "X-RateLimit-Remaining") "0")
                                                   (swap! rate-limit-reached (fn [_] true)))
                                                 (throw (proxy [Exception] [(str error-msg ". [" request-uri "]")]
                                                          (request [] (body "request"))
                                                          (remaining-requests [] (headers "X-RateLimit-Remaining"))
                                                          (rate-limit-reset [] (java.util.Date. 
                                                                                (headers "X-RateLimit-Reset")))))))))))
  ([type rest-uri]
     (condp = type
       :search (binding [*twitter-uri* (*settings* :search-uri)]
                 (twitter-request rest-uri))
       (twitter-request rest-uri))))

(defn id
  "ID of a user given screen name.

Note: User IDs in the regular twitter API appear to differ from those 
in the Search API."
  [name]
  (-> (twitter-request (str "/users/show.json?screen_name=" name))
      (get "id")))

(defn name
  "Name of a user given user id."
  [id]
  (-> (twitter-request (str "/users/show.json?user_id=" id))
      (get "screen_name")))

(defn friends
  "IDs of a user's friends."
  [id]
  (twitter-request (str "/friends/ids.json?user_id=" id)))

(defn followers 
  "IDs of a user's followers."
  [id]
  (try 
   (twitter-request (str "/followers/ids.json?user_id=" id))
   (catch Exception e
     (if (== 0 (((proxy-mappings e) "remaining-requests") e))
       (throw e)
       nil))))

(defn posts
  ([]
     (twitter-request (str "/statuses/public_timeline.json")))
  ([user-id]
     (twitter-request (str "/statuses/user_timeline.json?user_id=" user-id))))

(defn rate-limit-info
  []
  (twitter-request "/account/rate_limit_status.json"))

(defn search
  "Run a search query."
  ([query]
     (search query 100 1))
  ([query rpp]
     (search query rpp 1))
  ([query rpp page]
     (twitter-request :search (str "/search.json?q=" query "&rpp=" rpp "&page=" page))))