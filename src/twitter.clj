(ns twitter
  (:use [clojure.contrib.json :only [read-json]]
        [clojure.contrib.str-utils :only [re-gsub]]
        [clojure.contrib.java-utils :only [as-str]])
  (:require [clojure.set :as set]
            [com.twinql.clojure.http :as http]
            [twitter.query :as query]
            [oauth.client :as oauth])
  (:import (java.io File)
           (org.apache.http.entity.mime.content FileBody)
           (org.apache.http.entity.mime MultipartEntity)))

(declare status-handler)

(def *oauth-consumer* nil)
(def *oauth-access-token* nil)
(def *oauth-access-token-secret* nil)
(def *protocol* "http")

;; Get JSON from clj-apache-http 
(defmethod http/entity-as :json [entity as]
  (read-json (http/entity-as entity :string)))

(defmacro with-oauth
  "Set the OAuth access token to be used for all contained Twitter requests."
  [consumer access-token access-token-secret & body]
  `(binding [*oauth-consumer* ~consumer
             *oauth-access-token* ~access-token
             *oauth-access-token-secret* ~access-token-secret]
     (do 
       ~@body)))

(defmacro with-https
  [body]
  `(binding [*protocol* "https"]
     (do 
       ~@body)))

(defmacro def-twitter-method
  "Given basic specifications of a Twitter API method, build a function that will
take any required and optional arguments and call the associated Twitter method."
  [method-name req-method req-url required-params optional-params handler]
  (let [required-fn-params (vec (sort (map #(symbol (name %))
                                           required-params)))
        optional-fn-params (vec (sort (map #(symbol (name %))
                                           optional-params)))]
    `(defn ~method-name
       [~@required-fn-params & rest#]
       (let [req-uri# (str *protocol* "://" ~req-url)
             rest-map# (apply hash-map rest#)
             provided-optional-params# (set/intersection (set ~optional-params)
                                                         (set (keys rest-map#)))
             query-param-names# (sort (map (fn [x#] (keyword (re-gsub #"-" "_" (name x#))))
                                      (concat ~required-params provided-optional-params#)))
             query-params# (apply hash-map (interleave query-param-names#
                                                       (vec (concat ~required-fn-params
                                                                    (vals (sort (select-keys rest-map# 
                                                                                             provided-optional-params#)))))))
             oauth-creds# (when (and *oauth-consumer* 
                                     *oauth-access-token*) 
                            (oauth/credentials *oauth-consumer*
                                               *oauth-access-token*
                                               *oauth-access-token-secret*
                                               ~req-method
                                               req-uri#
                                               (into {} (map (fn [[k# v#]] [k# (oauth/url-encode v#)]) query-params#))))]
         ; (into {} (map (fn [k# v#] [k# (oauth/url-encode v#)]) query-params#))
         (~handler (~(symbol "http" (name req-method))
                    req-uri#
                    :query (merge query-params#
                                  oauth-creds#)
                    :parameters (http/map->params 
                                 {:use-expect-continue false})
                    :as :json))))))

;;;; Almost every method, and all functionality, of the Twitter API
;;;; is defined below with def-twitter-method or a custom function to support
;;;; special cases, such as uploading image files.

(def-twitter-method public-timeline
  :get
  "twitter.com/statuses/public_timeline.json"
  []
  []
  (comp #(:content %) status-handler))

(def-twitter-method friends-timeline
  :get
  "twitter.com/statuses/friends_timeline.json"
  []
  [:since-id
   :max-id
   :count
   :page]
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

(def-twitter-method home-timeline
  :get
  "twitter.com/statuses/home_timeline.json"
  []
  [:since-id
   :max-id
   :count
   :page]
  (comp #(:content %) status-handler))

(def-twitter-method mentions
  :get
  "twitter.com/statuses/mentions.json"
  []
  [:since-id
   :max-id
   :count
   :page]
  (comp #(:content %) status-handler))

(def-twitter-method show-status
  :get
  "twitter.com/statuses/show.json"
  [:id]
  []
  (comp #(:content %) status-handler))

(def-twitter-method update-status
  :post
  "twitter.com/statuses/update.json"
  [:status]
  [:in-reply-to-status-id]
  (comp #(:status (:content %)) status-handler))

(def-twitter-method destroy-status
  :post
  "twitter.com/statuses/destroy.json"
  [:id]
  []
  (comp #(:status (:content %)) status-handler))

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

(def-twitter-method direct-messages
  :get
  "twitter.com/direct_messages.json"
  []
  [:since-id
   :max-id
   :count
   :page]
  (comp #(:content %) status-handler))

(def-twitter-method sent-direct-messages
  :get
  "twitter.com/direct_messages/sent.json"
  []
  [:since-id
   :max-id
   :count
   :page]
  (comp #(:content %) status-handler))

(def-twitter-method send-direct-message-to-id
  :post
  "twitter.com/direct_messages/new.json"
  [:user-id
   :text]
  []
  (comp #(:content %) status-handler))

(def-twitter-method send-direct-message-to-name
  :post
  "twitter.com/direct_messages/new.json"
  [:screen-name
   :text]
  []
  (comp #(:content %) status-handler))

(def-twitter-method destroy-direct-message
  :post
  "twitter.com/direct_messages/destroy.json"
  [:id]
  []
  (comp #(:content %) status-handler))

(def-twitter-method create-friendship-to-id
  :post
  "twitter.com/friendships/create.json"
  [:user-id]
  [:follow]
  (comp #(:content %) status-handler))

(def-twitter-method create-friendship-to-name
  :post
  "twitter.com/friendships/create.json"
  [:screen-name]
  [:follow]
  (comp #(:content %) status-handler))

(def-twitter-method destroy-friendship-to-id
  :post
  "twitter.com/friendships/destroy.json"
  [:user-id]
  []
  (comp #(:content %) status-handler))

(def-twitter-method destroy-friendship-to-name
  :post
  "twitter.com/friendships/destroy.json"
  [:screen-name]
  []
  (comp #(:content %) status-handler))

(def-twitter-method show-friendship-by-ids
  :get
  "twitter.com/friendships/show.json"
  [:source-id
   :target-id]
  []
  (comp #(:content %) status-handler))

(def-twitter-method show-friendship-by-names
  :get
  "twitter.com/friendships/show.json"
  [:source-screen-name
   :target-screen-name]
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

(def-twitter-method verify-credentials
  :get
  "twitter.com/account/verify_credentials.json"
  []
  []
  (comp #(:content %) status-handler))

(def-twitter-method rate-limit-status
  :get
  "twitter.com/account/rate_limit_status.json"
  []
  []
  (comp #(:content %) status-handler))

(def-twitter-method end-session
  :post
  "twitter.com/account/end_session.json"
  []
  []
  (comp #(:content %) status-handler))

(def-twitter-method update-delivery-device
  :post
  "twitter.com/account/update_delivery_device.json"
  [:device]
  []
  (comp #(:content %) status-handler))

(def-twitter-method update-profile-colors
  :post
  "twitter.com/account/update_profile_colors.json"
  []
  [:profile-background-color
   :profile-text-color
   :profile-link-color
   :profile-sidebar-fill-color
   :profile-sidebar-border-color]
  (comp #(:content %) status-handler))


(comment (def-twitter-method update-profile-image
           :post
           "twitter.com/account/update_profile_image.json"
           [:image]
           []
           (comp #(:content %) status-handler)))

(defn update-profile-image [image]
  (let [req-uri__9408__auto__ "http://twitter.com/account/update_profile_image.json"
  
        oauth-creds__9414__auto__ (when
                                      (and
                                       *oauth-consumer*
                                       *oauth-access-token*)
                                    (oauth/credentials
                                     *oauth-consumer*
                                     *oauth-access-token*
                                     :post
                                     req-uri__9408__auto__))]
    ((comp #(:content %) status-handler)
     (http/post
      req-uri__9408__auto__
      :query
      oauth-creds__9414__auto__
      :parameters
      (http/map->params {:use-expect-continue false})
      :body (doto (MultipartEntity.)
              (.addPart "image" (FileBody. (File. image))))
      :as
      :json))))

(comment (def-twitter-method update-profile-background-image
           :post
           "twitter.com/account/update_profile_background_image.json"
           [:image]
           [:title]
           (comp #(:content %) status-handler)))

(defn update-profile-background-image [image & rest__2570__auto__]
  (let [req-uri__2571__auto__ "http://twitter.com/account/update_profile_background_image.json"
                              rest-map__2572__auto__ (apply hash-map rest__2570__auto__)
                              provided-optional-params__2573__auto__ (set/intersection
                                                                      (set [:title])
                                                                       (set
                                                                        (keys
                                                                         rest-map__2572__auto__)))
                              query-param-names__2574__auto__ (sort
                                                               (map
                                                                (fn 
                                                                 [x__2575__auto__]
                                                                 (keyword
                                                                  (re-gsub
                                                                   #"-"
                                                                   "_"
                                                                   (name
                                                                    x__2575__auto__))))
                                                                provided-optional-params__2573__auto__))
                              query-params__2576__auto__ (apply
                                                          hash-map
                                                           (interleave
                                                            query-param-names__2574__auto__
                                                            (vec
                                                             (vals
                                                              (sort
                                                               (select-keys
                                                                rest-map__2572__auto__
                                                                provided-optional-params__2573__auto__))))))
                              oauth-creds__2577__auto__ (when
                                                            (and
                                                             *oauth-consumer*
                                                             *oauth-access-token*)
                                                          (oauth/credentials
                                                           *oauth-consumer*
                                                           *oauth-access-token*
                                                           :post
                                                           req-uri__2571__auto__
                                                           query-params__2576__auto__))]
    ((comp #(:content %) status-handler)
     (http/post req-uri__2571__auto__
                :query (merge query-params__2576__auto__ oauth-creds__2577__auto__)
                :parameters (http/map->params {:use-expect-continue false})
                :body (doto (MultipartEntity.)
                        (.addPart "image" (FileBody. (File. image))))
                :as :json))))

(def-twitter-method update-profile
  :post
  "twitter.com/account/update_profile.json"
  []
  [:name 
   :email
   :url
   :location
   :description]
  (comp #(:content %) status-handler))

(def-twitter-method favorites
  :get
  "twitter.com/favorites.json"
  []
  [:id
   :page]
  (comp #(:content %) status-handler))

(def-twitter-method create-favorite
  :post
  "twitter.com/favorites/create.json"
  [:id]
  []
  (comp #(:content %) status-handler))

(def-twitter-method destroy-favorite
  :post
  "twitter.com/favorites/destroy.json"
  [:id]
  []
  (comp #(:content %) status-handler))

(def-twitter-method notifications-follow-by-id
  :post
  "twitter.com/notifications/follow.json"
  [:user-id]
  []
  (comp #(:content %) status-handler))

(def-twitter-method notifications-follow-by-name
  :post
  "twitter.com/notifications/follow.json"
  [:screen-name]
  []
  (comp #(:content %) status-handler))

(def-twitter-method notifications-leave-by-id
  :post
  "twitter.com/notifications/leave.json"
  [:user-id]
  []
  (comp #(:content %) status-handler))

(def-twitter-method notifications-leave-by-name
  :post
  "twitter.com/notifications/leave.json"
  [:screen-name]
  []
  (comp #(:content %) status-handler))

(def-twitter-method create-block
  :post
  "twitter.com/blocks/create.json"
  [:user-id-or-screen-name]
  []
  (comp #(:content %) status-handler))

(def-twitter-method destroy-block
  :post
  "twitter.com/blocks/destroy.json"
  [:user-id-or-screen-name]
  []
  (comp #(:content %) status-handler))

(def-twitter-method block-exists-for-id
  :get
  "twitter.com/blocks/exists.json"
  [:user-id]
  []
  (comp #(:content %) status-handler))

(def-twitter-method block-exists-for-name
  :get
  "twitter.com/blocks/exists.json"
  [:screen-name]
  []
  (comp #(:content %) status-handler))

(def-twitter-method blocking-users
  :get
  "twitter.com/blocks/blocking.json"
  []
  [:page]
  (comp #(:content %) status-handler))

(def-twitter-method blocking-user-ids
  :get
  "twitter.com/blocks/blocking/ids.json"
  []
  []
  (comp #(:content %) status-handler))

(def-twitter-method saved-searches
  :get
  "twitter.com/saved_searches.json"
  []
  []
  (comp #(:content %) status-handler))

(def-twitter-method show-saved-search
  :get
  "twitter.com/saved_searches/show.json"
  [:id]
  []
  (comp #(:content %) status-handler))

(def-twitter-method create-saved-search
  :post
  "twitter.com/saved_searches/create.json"
  [:query]
  []
  (comp #(:content %) status-handler))

(def-twitter-method destroy-saved-search
  :post
  "twitter.com/saved_searches/destroy.json"
  [:id]
  []
  (comp #(:content %) status-handler))

(def-twitter-method search
  :get
  "search.twitter.com/search.json"
  [:q]
  [:callback
   :lang
   :rpp
   :page
   :since-id
   :geocode
   :show-user]
  (comp #(:content %) status-handler))

(def-twitter-method trends
  :get
  "search.twitter.com/trends.json"
  []
  []
  (comp #(:content %) status-handler))

(def-twitter-method current-trends
  :get
  "search.twitter.com/trends/current.json"
  []
  [:exclude]
  (comp #(:content %) status-handler))

(def-twitter-method daily-trends
  :get
  "search.twitter.com/trends/daily.json"
  []
  [:date
   :exclude]
  (comp #(:content %) status-handler))

(def-twitter-method weekly-trends
  :get
  "search.twitter.com/trends/weekly.json"
  []
  [:date
   :exclude]
  (comp #(:content %) status-handler))

(defn status-handler
  "Handle the various HTTP status codes that may be returned when accessing
the Twitter API."
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

(defn make-rate-limit-handler
  "Creates a handler that will only be called if the API rate limit has been exceeded."
  [handler-fn]
  (fn [result]
    (let [headers (into {} (:headers result))]
      (when (= (headers "X-RateLimit-Remaining") "0")
        (handler-fn result)))))
