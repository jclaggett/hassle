(ns net.n01se.hassle.hw
  (:require [net.n01se.hassle.core :as hassle
             :refer [outputs
                     engine
                     node
                     output
                     output-handler
                     input
                     input-handler
                     net]]
            [clojure.core.async :as cca]))

(def ch-1 (cca/chan))
(defmethod input-handler ::ch-1 [_]
   ch-1)

(def main1
  (-> (input ::ch-1)
      (node (map #(str "Hello " %)))
      (output :stdout)
      outputs))

(def main2
  (-> (input :init)
      (node (map #(-> % :env (get "USER"))))
      (node (map #(str "Hello " %)))
      (output :stdout)
      outputs))

(def ch-3 (cca/chan))
(defmethod input-handler ::ch-3 [_] ch-3)

(def main3
  (-> (input ::ch-3)
      (node (take 2))
      (node (map #(str "Hello " %)))
      (output :stdout)
      outputs))

(def ch-4 (cca/chan))
(defmethod input-handler ::ch-4 [_] ch-4)

(def main4
  (let [a (input ::ch-4)
        b (node a (take 2))
        c (node b (map #(str "Hello " %)))
        d (node a (map #(str "Goodbye " %)))
        e (output #{c d} :stdout)]
    (outputs e)))

(def ch-5 (cca/chan))
(defmethod input-handler ::ch-5 [_] ch-5)

(def main5
  (let [a (input ::ch-5)
        b (node a (take 2))
        c (node b (map #(str "Hello " %)))
        d (node a (map #(str "Goodbye " %)))
        e (node #{c d} (map #(str % "!")))
        f (output #{d e} :stdout)]
    (outputs f)))

(defmethod input-handler ::dir
  [[_ dir]]
  (cca/to-chan!! (file-seq (clojure.java.io/file dir))))

(defmethod output-handler ::engine
  [_]
  (cca/chan 1 (map engine)))

(def fake-long "-rw-r--r-- 1 jclaggett jclaggett ")

(def ls
  (-> (input :init)
      (node
        (map (fn [{:keys [argv env]}]
               (clojure.pprint/pprint argv)
               (let [long? (= "-l" (get argv 0))
                     dir (get argv
                              (if long? 1 0)
                              (get env "PWD" "."))]
                 [long? dir]))))
      (node
        (map (fn [[long? dir]]
               (-> (input ::dir dir)
                   (node (map #(str (if long? fake-long "") %)))
                   (output :stdout)
                   outputs))))
      (output ::engine)
      outputs))

#_(defmethod hassle/input-handler ::dir-cmd [_] dir-cmd-chan)
#_(defmethod hassle/output-handler ::dir-cmd [_] dir-cmd-chan)

(defmethod hassle/io-chan ::dir-cmd [_]
  (cca/chan
    1
    (mapcat
      (fn [[long? dir]]
        (for [file (file-seq (clojure.java.io/file dir))]
          [long? file])))))
(def ls2
  (let [src-1 (input :init)
        src-2 (input ::dir-cmd)
        lnk-1 (node src-1
                   (map (fn [{:keys [argv env]}]
                          (let [long? (= "-l" (get argv 0))
                                dir (get argv
                                         (if long? 1 0)
                                         (get env "PWD" "."))]
                            [long? dir]))))
        lnk-2 (node src-2 (map (fn [[long? file]]
                                 (str (if long? fake-long "") file))))
        snk-1 (output lnk-1 ::dir-cmd)
        snk-2 (output lnk-2 :stdout)]
    #{snk-1 snk-2}))

(def ls3
  (net
    {src-1 :init
     src-2 ::dir-cmd}
    lnk-1 (node src-1
                (map (fn [{:keys [argv env]}]
                       (let [long? (= "-l" (get argv 0))
                             dir (get argv
                                      (if long? 1 0)
                                      (get env "PWD" "."))]
                         [long? dir]))))
    lnk-2 (node src-2 (map (fn [[long? file]]
                             (str (if long? fake-long "") file))))
    {::dir-cmd lnk-1
     :stdout lnk-2}))

(def net1
  (net {} {}))

(def net2
  (net {init :init}
       {:stdout init}))

(def net3
  (net {init :init}
       argv (node init (map :argv))
       {:stdout argv}))
