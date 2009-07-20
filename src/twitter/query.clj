(ns twitter.query
  (:refer-clojure :exclude [or name])
  (:require [clojure.contrib.str-utils :as str-utils]))

(defn or
  "Build up a search string where all terms are OR'd."
  [& terms]
  (java.net.URLEncoder/encode 
   (str-utils/str-join " OR " 
                       (map #(str "\"" % "\"") terms))
   "UTF-8"))
