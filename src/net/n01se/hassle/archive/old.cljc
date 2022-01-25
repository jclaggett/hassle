(ns net.n01se.hassle.core
  (:require [lazy-map.core :as lm]
            [clojure.core.async :as cca]
            [clojure.pprint :refer [pprint]]))

;; Try 1. Just use Lazy Seqs
(comment
  (fn engine [main]
      (let [sources ({:init [init]})
            sinks (main sources)]

        (let [unknown-keys (clojure.set/difference known-sink-keys (set (keys sinks)))]
          (assert (empty? unknown-keys) (str "Unsupported sink keys: " unknown-keys)))

        (doseq [s (:stdout sinks)]
          (println s))))

  (def engine (init-engine
                {:argv ["sterrett"]
                 :env (into {} (System/getenv))})))

;; Try 2. Build a DAG of streams based on the return value from main

#_(defn build-streams [spec]
    #_"first, check for unknown keys"
    #_"second, build each root sink"
    #_"topo sort the spec in breadth first order"
    #_"Iterate through each stream-spec creating each stream and hooking up to that stream")
  
;; Try 3. Build a DAG of async channels
(defn async-head [ch] (cca/mult ch))
(defn async-link [m xf] (cca/mult (cca/tap m (cca/chan 1 xf))))
(defn async-join [ms] (cca/mult (cca/merge (map #(cca/tap % (cca/chan)) ms))))
(defn async-drain [ms] (map #(cca/tap % (cca/chan (cca/dropping-buffer 0))) ms))

;; Test range
(defn test-4 [ch]
  (let [a (async-head ch)
        b (async-link a (map #(Integer/parseInt %)))
        c (async-link b (map inc))
        d (async-link b (map #(* 2 %)))
        e (async-join [c d])
        f (async-link e (map inc))
        g (async-link f (map #(doto % println)))
        h (async-drain [g])]
    ch))

(defn test-3 [ch]
  (let [a (async-head ch)
        b (async-link a (map #(Integer/parseInt %)))
        c (async-link b (map #(doto % println)))
        d (async-drain [c])]
    ch))

(defn test-2 [ch]
  (let [a (async-head ch)
        b (async-link a (map #(doto % println)))
        c (async-drain [b])]
    ch))

(defn test-1 [ch]
  (let [a (async-head ch)
        b (async-drain [a])]
    ch))

(defn run [tst coll] (cca/onto-chan! (tst (cca/chan)) coll) nil)
