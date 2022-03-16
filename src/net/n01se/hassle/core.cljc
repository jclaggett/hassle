(ns net.n01se.hassle.core
  (:require [clojure.pprint :refer [pprint]]

            [net.n01se.hassle.xfnet :as xfn]))

(defn debug
  ([x] (debug x x))
  ([msg x] (println "DEBUG:" msg) x))

