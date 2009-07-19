(ns twitter.query
  (:refer-clojure :exclude [or name])
  (:use [clojure.http.client])
  (:require [clojure.http.resourcefully :as resourcefully]
            [clojure.contrib.str-utils :as str-utils]))

(defn or
  "Build up a search string where all terms are OR'd."
  [& terms]
  (url-encode 
   (str-utils/str-join " OR " 
                       (map #(str "\"" % "\"") terms))))
