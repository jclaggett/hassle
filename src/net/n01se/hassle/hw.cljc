(ns net.n01se.hassle.hw
  (:require [clojure.pprint :refer [pprint]]

            [net.n01se.hassle.transducers
             :refer [tag input inputs node output outputs embed]]))

(defn run-ex [xf]
  (sequence xf (-> xf meta ::ts)))

(def ex1
  (vary-meta
    (input :foo)
    assoc ::ts (tag :foo (range 3))))

(def ex2
  (vary-meta
    (->> (input :init)
         (output :stdout))
    assoc ::ts (tag :init (range 3))))

(def ex3
  (vary-meta
    (->> (input :init)
         (node (map :argv))
         (output :stdout))
    assoc ::ts (tag :init [{:argv ["ls" "-l"]}])))

(def ex4
  (vary-meta
    (->> (input ::ch)
         (node (map #(str "Hello " %)))
         (output :stdout))
    assoc ::ts (tag ::ch ["bob" "jim" "joe"])))

(def ex5
  (vary-meta
    (->> (input :init)
         (node (map #(-> % :env (get "USER"))))
         (node (map #(str "Hello " %)))
         (output :stdout))
    assoc ::ts (tag :init [{:argv ["ls" "-l"]
                            :env (System/getenv)}])))

(def ex6
  (vary-meta
    (->> (input ::ch)
         (node (take 2))
         (node (map #(str "Hello " %)))
         (output :stdout))
    assoc ::ts (tag ::ch ["bob" "jim" "joe"])))

(def ex7
  (vary-meta
    (let [{i ::ch} inputs
          a (node (map #(str "Hello " %)) i)
          b (node (map #(str "Goodbye " %)) i)]
      (outputs {:stdout #{a b}}))
    assoc ::ts (tag ::ch ["bob" "jim" "joe"])))

(def ex8
  (vary-meta
    (let [{i ::ch} inputs
          a (node (map #(str "Goodbye " %)) i)
          b (node (take 2) i)
          c (node (map #(str "Hello " %)) b)]
      (outputs {:stdout #{a c}}))
    assoc ::ts (tag ::ch ["bob" "jim" "joe"])))

(def ex9
  (vary-meta
    (let [{i ::ch} inputs
          a (node (map #(str "Goodbye " %)) i)
          {c :stdout} (embed ex6 {::ch i})]
      (outputs {:stdout #{a c}}))
    assoc ::ts (tag ::ch ["bob" "jim" "joe"])))

(def ex10
  (vary-meta
    (let [{ch ::ch} inputs
          b (node (take 2) ch)
          c (node (map #(str "Hello " %)) b)
          d (node (map #(str "Goodbye " %)) ch)
          e (node (map #(str % "!")) #{c d})]
      (outputs {:stdout #{d e}}))
    assoc ::ts (tag ::ch ["bob" "jim" "joe"])))

(def ex11
  (vary-meta
    (let [{a ::ch1
           b ::ch2} inputs
          c (node (take 2) #{a b})]
      (outputs {:stdout c}))
    assoc ::ts (interleave
                 (tag ::ch1 ["bob" "jim" "joe"])
                 (tag ::ch2 ["sue" "kim" "meg"]))))

(def ex12
  (vary-meta
    (let [{a ::ch1} inputs
          b (node identity a)]
      (outputs {:stdout #{a b}}))
    assoc ::ts (tag ::ch1 ["bob" "jim" "joe"])))

(defn suffix
  ([s input-xfs xs] (sequence (suffix s input-xfs) xs))
  ([s input-xfs] (node (suffix s) input-xfs))
  ([s] (map #(str % s))))

(def ex13
  (vary-meta
    (let [{a :stdin} inputs
          b (suffix "-b" a)
          c (suffix "-c" #{a b})]
       (outputs {:stdout #{a c}}))
    assoc ::ts (tag :stdin ["bob" "jim" "joe"])))

(def ex14
  (vary-meta
    (let [{c1 ::ch1
           c2 ::ch2} inputs
          n1 (suffix "-n1" c1)
          n2 (suffix "-n2" c2)
          n3 (suffix "-n3" n1)
          n4 (node (comp (suffix "-n4") (take 2)) #{n1 n2})]
      (outputs {:stdout #{n3 n4}}))
    assoc ::ts (interleave
                 (tag ::ch1 ["bob" "jim" "joe"])
                 (tag ::ch2 ["sue" "kim" "meg"]))))

(comment
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
