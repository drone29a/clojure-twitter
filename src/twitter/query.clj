(ns twitter.query
  (:refer-clojure :exclude [or name])
  (:require [clojure.string :as string]))

(defn or
  "Build up a search string where all terms are OR'd."
  [& terms]
  (java.net.URLEncoder/encode 
   (string/join " OR " 
                (map #(str "\"" % "\"") terms))
   "UTF-8"))
