(ns net.n01se.hassle.hw
  (:require [net.n01se.hassle.core :as hassle
             :refer [drain
                     engine
                     link
                     sink
                     sink-handler
                     source
                     source-handler]]
            [clojure.core.async :as cca]))

(def ch-1 (cca/chan))
(defmethod source-handler ::ch-1 [_]
   ch-1)

(def main1
  (-> (source ::ch-1)
      (link (map #(str "Hello " %)))
      (sink :stdout)
      drain))

(def main2
  (-> (source :init)
      (link (map #(-> % :env (get "USER"))))
      (link (map #(str "Hello " %)))
      (sink :stdout)
      drain))

(def ch-3 (cca/chan))
(defmethod source-handler ::ch-3 [_] ch-3)

(def main3
  (-> (source ::ch-3)
      (link (take 2))
      (link (map #(str "Hello " %)))
      (sink :stdout)
      drain))

(def ch-4 (cca/chan))
(defmethod source-handler ::ch-4 [_] ch-4)

(def main4
  (let [a (source ::ch-4)
        b (link a (take 2))
        c (link b (map #(str "Hello " %)))
        d (link a (map #(str "Goodbye " %)))
        e (sink #{c d} :stdout)]
    (drain e)))

(def ch-5 (cca/chan))
(defmethod source-handler ::ch-5 [_] ch-5)

(def main5
  (let [a (source ::ch-5)
        b (link a (take 2))
        c (link b (map #(str "Hello " %)))
        d (link a (map #(str "Goodbye " %)))
        e (link #{c d} (map #(str % "!")))
        f (sink #{d e} :stdout)]
    (drain f)))

(defmethod source-handler ::dir
  [[_ dir]]
  (cca/to-chan!! (file-seq (clojure.java.io/file dir))))

(defmethod sink-handler ::engine
  [_]
  (cca/chan 1 (map engine)))

(def fake-long "-rw-r--r-- 1 jclaggett jclaggett ")

(def ls
  (-> (source :init)
      (link
        (map (fn [{:keys [argv env]}]
               (let [long? (= "-l" (get argv 0))
                     dir (get argv
                              (if long? 1 0)
                              (get env "PWD" "."))]
                 [long? dir]))))
      (link
        (map (fn [[long? dir]]
               (-> (source ::dir dir)
                   (link (map #(str (if long? fake-long "") %)))
                   (sink :stdout)
                   drain))))
      (sink ::engine)
      drain))
