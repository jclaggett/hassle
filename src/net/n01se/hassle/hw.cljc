(ns net.n01se.hassle.hw
  (:require [clojure.pprint :refer [pprint]]

            [net.n01se.hassle.transducers :as t]
            [net.n01se.hassle.net :as n]))

(def ex1
  (n/net #{}))

(def ex2
  (->> (n/input :init)
       (n/output :stdout)
       n/net))

(def ex3
  (->> (n/input :init)
       (n/node (map :argv))
       (n/output :stdout)
       n/net))

(def ex4
  (->> (n/input ::ch)
       (n/node (map #(str "Hello " %)))
       (n/output :stdout)
       n/net))

(def ex5
  (->> (n/input :init)
       (n/node (map #(-> % :env (get "USER"))))
       (n/node (map #(str "Hello " %)))
       (n/output :stdout)
       n/net))

(def ex6
  (->> (n/input ::ch)
       (n/node (take 2))
       (n/node (map #(str "Hello " %)))
       (n/output :stdout)
       n/net))

(def ex7
  (let [{i ::ch} n/inputs
        a (n/node (map #(str "Hello " %)) i)
        b (n/node (map #(str "Goodbye " %)) i)
        o (n/outputs {:stdout #{a b}})]
    (n/net o)))

(def ex8
  (let [{i ::ch} n/inputs
        a (n/node (map #(str "Goodbye " %)) i)
        b (n/node (take 2) i)
        c (n/node (map #(str "Hello " %)) b)
        o (n/outputs {:stdout #{a c}})]
    (n/net o)))

(def ex9
  (let [{i ::ch} n/inputs
        a (n/node (map #(str "Goodbye " %)) i)
        {c :stdout} (ex3 {:init i})
        o (n/outputs {:stdout #{a c}})]
    o #_(n/net o)))

(comment
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

  #_
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
       :stdout files})))
