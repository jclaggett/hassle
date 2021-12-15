(ns net.n01se.hassle.hw
  (:require [net.n01se.hassle.core :as h
             :refer [start node net]]
            [clojure.core.async :as cca]
            [clojure.pprint :refer [pprint]]))

(defmethod h/io-chan ::ch [_] (cca/chan))

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

(def ex11
  (net {a ::ch1
        b ::ch2}
       c (node #{a b} (take 2))
       {:stdout c}))

(def ex12
  (net {a ::ch1}
       b (node a identity)
       {:stdout #{a b}}))

(def ex13
  (net {a :stdin}
       b (node a (map #(str % "-b")) 'b)
       c (node #{a b} (map #(str % "-c")) 'c)
       {:stdout #{a c}}))

(def ex14
  (net {c1 ::ch1
        c2 ::ch2}
       n1 (node c1 (map #(str % "-n1")))
       n2 (node c2 (map #(str % "-n2")))
       n3 (node n1 (map #(str % "-n3")))
       n4 (node #{n1 n2} (comp (map #(str % "-n4")) (take 2)))
       {:stdout #{n3 n4}}))


(def fake-long "-rw-r--r-- 1 jclaggett jclaggett ")

(def dir-cmd-xf
  (mapcat
    (fn [[long? dir]]
      (println "dir:" dir)
      (for [file (file-seq (clojure.java.io/file dir))]
        (do
          (println "file: " file)
          [long? file])))))

(defmethod h/io-chan ::dir-cmd [_]
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

