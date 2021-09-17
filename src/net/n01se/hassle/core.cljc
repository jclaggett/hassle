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

;; Try 4. define a DAG data structure
(defn source [& args] (concat [[] :source] args))
(defn link [prev & args] (concat [(if (set? prev) prev #{prev}) :link] args))
(defn sink [prev & args] (concat [(if (set? prev) prev #{prev}) :sink] args))
(defn drain [prev] [prev :drain])

(defn merge-deps [deps]
  (if (= (count deps) 1)
    (first deps)
    (cca/merge
      (for [dep deps]
        (if (satisfies? cca/Mult dep)
          (cca/tap dep (cca/chan))
          dep)))))

(defn connect-dep [dep ch]
  (if (satisfies? cca/Mult dep)
    (cca/tap dep ch)
    (cca/pipe dep ch)))

(defn mult-node [ch node]
  (if (> (count (:next node)) 1)
    (cca/mult ch)
    ch))

(defmulti asyncify :type)
(defmulti source-handler first)
(defmulti sink-handler first)

(defmethod asyncify :source
  [{args :args :as x}]
  (mult-node (source-handler args) x))

(defmethod asyncify :link
  [{deps :deps [xf] :args :as x}]
  (-> (merge-deps deps)
      (connect-dep (cca/chan 1 xf))
      (mult-node x)))

(defmethod asyncify :sink
  [{deps :deps args :args}]
  (-> (merge-deps deps)
      (connect-dep (sink-handler args))))
        

(defmethod source-handler :init
  [_]
  (cca/to-chan!
    [ {:argv ["sterrett"]
       :env (into {} (System/getenv))}]))

(defmethod sink-handler :stdout
  [_]
  (cca/chan 1 (map #(doto % println))))

(defmethod asyncify :drain
  [{deps :deps}]
  (-> (merge-deps deps)
      (connect-dep (cca/chan (cca/dropping-buffer 0)))))

(defn make-rdag
  ([node] (make-rdag {::root (hash node)} node))
  ([rdag [prev-nodes node-type & args :as node]]
   (if (contains? rdag (hash node))
     rdag
     (reduce (fn [rdag prev-node]
               (-> rdag
                   (make-rdag prev-node)
                   (update-in [(hash prev-node) :next] conj (hash node))
                   (update-in [(hash node) :prev] conj (hash prev-node))))
             (assoc rdag (hash node) {:prev #{}
                                      :next #{}
                                      :id (hash node)
                                      :type node-type
                                      :args args})
             prev-nodes))))

(defn postwalk-rdag [orig-rdag kids-fn update-fn]
  (letfn [(update-node [rdag node-key]
            (update rdag node-key update-fn rdag))

          (visit-kids [rdag node-key]
            (reduce visit-node rdag (kids-fn (rdag node-key))))

          (visit-node [rdag node-key]
            (if (contains? (-> rdag meta ::visited) node-key)
              rdag
              (-> rdag
                  (vary-meta update ::visited conj node-key)
                  (visit-kids node-key)
                  (update-node node-key))))]
    (-> orig-rdag
        (vary-meta assoc ::visited #{})
        (visit-node (::root orig-rdag)))))

(defn lint-rdag [rdag]
  rdag)

(defn asyncify-rdag [rdag]
  (postwalk-rdag
    rdag
    :prev
    (fn [node rdag]
      (let [deps (->> (:prev node)
                      (map rdag)
                      (map :async))
            node (assoc node :deps deps)]
        (assoc node :async (asyncify node))))))

(def known-source-keys #{:chan :init})
(def known-sink-keys #{:stdout})
(defn engine [main]
  (-> (main)
      drain
      make-rdag
      lint-rdag
      asyncify-rdag))
