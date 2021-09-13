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
(defn async-tail [m] (cca/tap m (cca/chan (cca/dropping-buffer 0))))

;; Test range
(defn test-4 [ch]
  (let [a (async-head ch)
        b (async-link a (map #(Integer/parseInt %)))
        c (async-link b (map inc))
        d (async-link b (map #(* 2 %)))
        e (async-join [c d])
        f (async-link e (map inc))
        g (async-link f (map #(doto % println)))
        h (async-tail g)]
    ch))

(defn test-3 [ch]
  (let [a (async-head ch)
        b (async-link a (map #(Integer/parseInt %)))
        c (async-link b (map #(doto % println)))
        d (async-tail c)]
    ch))

(defn test-2 [ch]
  (let [a (async-head ch)
        b (async-link a (map #(doto % println)))
        c (async-tail b)]
    ch))

(defn test-1 [ch]
  (let [a (async-head ch)
        b (async-tail a)]
    ch))

(defn run [tst coll] (cca/onto-chan! (tst (cca/chan)) coll) nil)

;; Try 4. define a DAG data structure
(defn source [& args] (concat [[] :source] args))
(defn link [prev & args] (concat [[prev] :link] args))
(defn sink [prev & args] (concat [[prev] :sink] args))
(defn join [prevs] [prevs :join])
(defn tail [prevs] [prevs :tail])

(defmulti asyncify :type)

(defmethod asyncify :link
  [{[dep] :deps [xf] :args}] (async-link dep xf))

(defn drill-down [x]
  (-> x
      (update :type vector (-> x :args first))
      (update :args rest)
      asyncify))

(defmethod asyncify :source [x] (drill-down x))
(defmethod asyncify :sink [x] (drill-down x))

(defmethod asyncify [:source :chan]
  [{[ch] :args :as x}]
  (async-head ch))

(defmethod asyncify [:source :init]
  [_]
  (async-head
    (cca/to-chan!
      [ {:argv ["sterrett"]
         :env (into {} (System/getenv))}])))

(defmethod asyncify [:sink :stdout]
  [{[dep] :deps}]
  (async-link dep (map #(doto % println))))

(defmethod asyncify :join
  [{deps :deps}]
  (async-join deps))

(defmethod asyncify :tail
  [{[dep] :deps}]
  (async-tail dep))

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
  (let [t (tail (main))]
    (-> (main)
        tail
        make-rdag
        lint-rdag
        asyncify-rdag)))
