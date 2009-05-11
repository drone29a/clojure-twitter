;;; resourcefully.clj
;;
;; A wrapper around client.clj that's designed for accessing RESTful
;; APIs pleasantly.
;;
;; Since resourcefully defines a get function, you'll need to :use it
;; :as resourcefully rather than in an unqualified way.
;;
;; (ns your-ns
;;   (:use [clojure.http.resourcefully :as resourcefully]))
;;
;; (resourcefully/get  "http://clojure.org")
;; (with-cookies
;;   (resourcefully/post "https://www.google.com/accounts/LoginAuth" {}
;;                       {"Email" "clojure@gmail.com" "Passwd" "conj"})
;;   (resourcefully/get  "http://www.google.com/reader"))
;;

(ns clojure.http.resourcefully
  (:use [clojure.contrib.str-utils :only [str-join]])
  (:use [clojure.http.client :as client])
  (:refer-clojure :exclude [get]))

(def *cookies* nil)

(defn- save-cookies [response]
  (when (and *cookies* (:cookies response))
    (dosync (alter *cookies* merge (:cookies response))))
  response)

(defn- error? [response]
  (>= (:code response) 400))

(defn error-message [response]
  (str "Problem with " (:url response) ": "
       (:code response) " "
       (:msg response) "\n"
       (str-join "\n" (:body-seq response))))

(defmacro define-method
  [method]
  `(defn ~method
     ~(str "Perform HTTP " method " request to url u with specified headers
map. Cookies will be saved if inside with-cookies block.")
     [u# & [headers# body#]]
     (let [response# (save-cookies (client/request u# ~(str method)
                                                   headers# *cookies* body#))]
       (if (error? response#)
         (throw (java.io.IOException. (error-message response#)))
         response#))))

(define-method get)
(define-method put)
(define-method post)
(define-method delete)
(define-method head)

(defmacro with-cookies
  "Perform body with *cookies* bound to cookie-map. Responses that set
  cookies will have them saved in the *cookies* ref."
  [cookie-map & body]
  `(binding [*cookies* (ref (or cookie-map {}))]
     ~@body))
