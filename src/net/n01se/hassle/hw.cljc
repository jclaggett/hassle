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
      (hassle/link (map #(-> % :env (get "USER"))))
      (hassle/link (map #(str "Hello " %)))
      (hassle/sink :stdout)
      vector))

(def ch-3 (cca/chan))
(defn main3 []
  (-> (hassle/source :chan ch-3)
      (hassle/link (take 2))
      (hassle/link (map #(str "Hello " %)))
      (hassle/sink :stdout)
      vector))

(def ch-4 (cca/chan))
(defn main4 []
  (let [a (hassle/source :chan ch-4)
        b (hassle/link a (take 2))
        c (hassle/link b (map #(str "Hello " %)))
        d (hassle/link a (map #(str "Goodbye " %)))
        e (hassle/sink #{c d} :stdout)]
    [e]))
