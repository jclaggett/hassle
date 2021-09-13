(ns net.n01se.hassle.hw
  (:require [net.n01se.hassle.core :as hassle]
            [clojure.core.async :as cca]))

(def ch-1 (cca/chan))

(defn main1 []
  (-> (hassle/source :chan ch-1)
      (hassle/link (map #(str "Hello " %)))
      (hassle/sink :stdout)
      vector))

(defn main2 []
  (-> (hassle/source :init)
      (hassle/link (map #(str "Hello " (-> % :env (get "USER")))))
      (hassle/sink :stdout)
      vector))

#_
(defn main3 [sources]
  {:stdout (->> (:init sources)
                (map #(-> % :env (get "USER")))
                (map #(str "hello " %)))})

#_
(defn main4 [sources]
  {:stdout (->> (:stdin sources)
                (take 2)
                (map #(str "hello " %)))})
