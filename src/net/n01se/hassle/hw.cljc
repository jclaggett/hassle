(ns net.n01se.hassle.hw
  (:require [net.n01se.hassle.core :as hassle]
            [clojure.core.async :as cca]))

(def ch-1 (cca/chan))
(defmethod hassle/source-handler ::ch-1 [_]
   (clojure.pprint/pprint (str "inside " ::ch-1))
   ch-1)

(defn main1 []
  (-> (hassle/source ::ch-1)
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
(defmethod hassle/source-handler ::ch-3 [_] ch-3)

(defn main3 []
  (-> (hassle/source :ch-3)
      (hassle/link (take 2))
      (hassle/link (map #(str "Hello " %)))
      (hassle/sink :stdout)
      vector))

(def ch-4 (cca/chan))
(defmethod hassle/source-handler ::ch-4 [_] ch-4)

(defn main4 []
  (let [a (hassle/source ::ch-4)
        b (hassle/link a (take 2))
        c (hassle/link b (map #(str "Hello " %)))
        d (hassle/link a (map #(str "Goodbye " %)))
        e (hassle/sink #{c d} :stdout)]
    [e]))

(def ch-5 (cca/chan))
(defmethod hassle/source-handler ::ch-5 [_] ch-5)

(defn main5 []
  (let [a (hassle/source ::ch-5)
        b (hassle/link a (take 2))
        c (hassle/link b (map #(str "Hello " %)))
        d (hassle/link a (map #(str "Goodbye " %)))
        e (hassle/link #{c d} (map #(str % "!")))
        f (hassle/sink #{d e} :stdout)]
    [f]))
