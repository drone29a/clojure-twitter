(ns twitter
  (:use [clojure.contrib.json.read]
        [clojure.contrib.str-utils :only [re-gsub]]
        [clojure.contrib.java-utils :only [as-str]]
        [clojure.contrib.seq-utils :only [flatten]])
  (:require [clojure.set :as set]
            [com.twinql.clojure.http :as http]
            [twitter.query :as query]))

(def rate-limit-reached (atom nil))

(def *twitter-uri* nil)

(def *settings* {:basic-auth nil
                 :oauth-auth nil})

(declare status-handler)

(defmethod http/entity-as :json [entity as]
  (read-json (http/entity-as entity :string)))

(defmacro def-twitter-method
  [method-name req-method req-url required-params optional-params handler]
  (let [required-fn-params (vec (sort (map #(symbol (name %))
                                           required-params)))
        optional-fn-params (vec (sort (map #(symbol (name %))
                                           optional-params)))]
    `(defn ~method-name
       [~@required-fn-params & rest#]
       (let [rest-map# (apply hash-map rest#)
             provided-optional-params# (set/intersection (set ~optional-params)
                                                         (set (keys rest-map#)))
             query-params# (sort (map (fn [x#] (keyword (re-gsub #"-" "_" (name x#))))
                                      (concat ~required-params provided-optional-params#)))]
         (~handler (~(symbol "http" (name req-method))
                                  ~(str "http://" req-url)
                                  :query (apply hash-map (interleave query-params#
                                                                    (vec (concat ~required-fn-params
                                                                                 (vals (sort (select-keys rest-map# 
                                                                                                          provided-optional-params#)))))))
                                  :as :json))))))

(def-twitter-method update-status
  :post
  "twitter.com/statuses/update.json"
  [:status
   :in-reply-to-status-id]
  []
  (comp #(:status (:content %)) status-handler))

(comment (defn update-status
           [status in-reply-to-status-id]
           (apply (comp #(:status (:content %)) status-handler)
                  (http/post (str "http://" "twitter.com/statuses/update.json")
                             :query {:status status
                                     :in_reply_to_status_id in-reply-to-status-id}))))

(def-twitter-method show-status
  :get
  "twitter.com/statuses/show.json"
  [:id]
  []
  (comp #(:content %) status-handler))

(def-twitter-method show-user-by-id
  :get
  "twitter.com/users/show.json"
  [:user-id]
  []
  (comp #(:content %) status-handler))

(def-twitter-method show-user-by-name
  :get
  "twitter.com/users/show.json"
  [:screen-name]
  []
  (comp #(:content %) status-handler))

(def-twitter-method friends-of-id
  :get
  "twitter.com/friends/ids.json"
  [:user-id]
  []
  (comp #(:content %) status-handler))

(def-twitter-method friends-of-name
  :get
  "twitter.com/friends/ids.json"
  [:screen-name] 
  []
  (comp #(:content %) status-handler))

(def-twitter-method followers-of-id
  :get
  "twitter.com/followers/ids.json"
  [:user-id]
  []
  (comp #(:content %) status-handler))

(def-twitter-method followers-of-name
  :get
  "twitter.com/followers/ids.json"
  [:screen-name]
  []
  (comp #(:content %) status-handler))

(def-twitter-method public-timeline
  :get
  "twitter.com/statuses/public_timeline.json"
  []
  []
  (comp #(:content %) status-handler))

(def-twitter-method user-timeline
  :get
  "twitter.com/statuses/user_timeline.json"
  []
  [:id
   :user-id
   :screen-name
   :since-id
   :max-id
   :count
   :page]
  (comp #(:content %) status-handler))

(def-twitter-method rate-limit-status
  :get
  "twitter.com/account/rate_limit_status.json"
  []
  []
  (comp #(:content %) status-handler))

(defn search
  "Run a search query."
  ([query]
     (search query 100 1))
  ([query rpp]
     (search query rpp 1))
  ([query rpp page]
     (twitter-request :search (str "/search.json?q=" query "&rpp=" rpp "&page=" page))))

(defn status-handler
  [result]
  (condp #(if (coll? %1)  
            (first (filter (fn [x] (== x %2)) %1))
            (== %2 %1)) (:code result)
    200 result
    304 nil
    [400 401 403 404 406 500 502 503] (let [body (:content result)
                                            headers (into {} (:headers result))
                                            error-msg (body "error")
                                            request-uri (body "request")]
                                        (throw (proxy [Exception] [(str error-msg ". [" request-uri "]")]
                                                 (request [] (body "request"))
                                                 (remaining-requests [] (headers "X-RateLimit-Remaining"))
                                                 (rate-limit-reset [] (java.util.Date. 
                                                                       (headers "X-RateLimit-Reset"))))))))

(defn rate-limiter-shutoff-handler
  [result]
  (let [headers (into {} (:headers result))]
    (when (= (headers "X-RateLimit-Remaining") "0")
      (swap! rate-limit-reached (fn [_] true)))))