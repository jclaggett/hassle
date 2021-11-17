(ns net.n01se.hassle.hw
  (:require [net.n01se.hassle.core :as hassle
             :refer [start node net]]
            [clojure.core.async :as cca]
            [clojure.pprint :refer [pprint]]))

(defmethod hassle/io-chan ::ch [_] (cca/chan))

(def ex1
  (net {} {}))

(def ex2
  (net {init :init}
       {:stdout init}))

(def ex3
  (net {init :init}
       argv (node init (map :argv))
       {:stdout argv}))

(def ex4
  (net {input ::ch}
       greeting (node input (map #(str "Hello " %)))
       {:stdout greeting}))

(def ex5
  (net {input :init}
       user (node input (map #(-> % :env (get "USER"))))
       greeting (node user (map #(str "Hello " %)))
       {:stdout greeting}))

(def ex6
  (net {input ::ch}
       input-2 (node input (take 2))
       greeting (node input-2 (map #(str "Hello " %)))
       {:stdout greeting}))

(def ex7
  (net {input ::ch}
       a (node input (map #(str "Hello " %)))
       b (node input (map #(str "Goodbye " %)))
       {:stdout #{a b}}))

(def ex8
  (net {input ::ch}
       a (node input (map #(str "Goodbye " %)))
       b (node input (take 2))
       c (node b (map #(str "Hello " %)))
       {:stdout #{a c}}))

#_
(def ex9
  (net {input ::ch}
       a (node input (map #(str "Goodbye " %)))
       {c :stdout} (embed ex3 {::ch input})
       {:stdout #{a c}}))

(def ex10
  (net {input ::ch}
       b (node input (take 2))
       c (node b (map #(str "Hello " %)))
       d (node input (map #(str "Goodbye " %)))
       e (node #{c d} (map #(str % "!")))
       {:stdout #{d e}}))

(def fake-long "-rw-r--r-- 1 jclaggett jclaggett ")

(def dir-cmd-xf
  (mapcat
    (fn [[long? dir]]
      (println "dir:" dir)
      (for [file (file-seq (clojure.java.io/file dir))]
        (do
          (println "file: " file)
          [long? file])))))

(defmethod hassle/io-chan ::dir-cmd [_]
  (cca/chan 1 dir-cmd-xf))

(def ls
  (net
    {init :init
     dir-cmd ::dir-cmd}
    opts (node init
               (map (fn [{:keys [argv env]}]
                      (let [long? (= "-l" (get argv 0))
                            dir (get argv
                                     (if long? 1 0)
                                     (get env "PWD" "."))]
                        [long? dir]))))
    files (node dir-cmd (map (fn [[long? file]]
                               (str (if long? fake-long "") file))))
    {::dir-cmd opts
     :stdout files}))

