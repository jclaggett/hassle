(ns net.n01se.hassle.core
  (:require [lazy-map.core :as lm]
            [clojure.core.async :as cca]
            [clojure.pprint :refer [pprint]]))

(defn input [& args] (concat [:input #{}] args))
(defn link [& args] (cons :link args))
(defn output [& args] (cons :output args))
(defn outputs [& args] (cons :outputs args))

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
(defmulti input-handler first)
(defmulti output-handler first)

(defmethod asyncify :input
  [{args :args :as x}]
  (mult-node (input-handler args) x))

(defmethod asyncify :link
  [{deps :deps [xf] :args :as x}]
  (-> (merge-deps deps)
      (connect-dep (cca/chan 1 xf))
      (mult-node x)))

(defmethod asyncify :output
  [{deps :deps args :args}]
  (-> (merge-deps deps)
      (connect-dep (output-handler args))))
        

(def argv [])
(def env (into {} (System/getenv)))

(defmethod input-handler :init
  [_]
  (cca/to-chan! [{:argv argv :env env}]))

(defmethod output-handler :stdout
  [_]
  (cca/chan 1 (map #(doto % println))))

(defmethod asyncify :outputs
  [{deps :deps}]
  (-> (merge-deps deps)
      (connect-dep (cca/chan (cca/dropping-buffer 0)))))

(defn make-rdag
  ([node] (make-rdag {::root (hash node)} node))
  ([rdag [node-type prev-nodes & args :as node]]
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
             (if (set? prev-nodes)
               prev-nodes
               #{prev-nodes})))))

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

(defn engine [main]
  (-> main
      make-rdag
      lint-rdag
      asyncify-rdag))

(defmacro xnet [& args]
  (let [inputs (first args)
        typed-inputs (mapcat (fn [[k v]] [k (concat [`list :input #{}] v)]) inputs)
        body (drop-last (rest args))
        outputs (last args)
        typed-outputs (map (fn [[k v]] (list `list :output v k)) outputs)]
    `^{::xnet (let [~@typed-inputs ~@body] (list :outputs #{~@typed-outputs}))}
    (fn [~inputs] (let [~@body] ~outputs))))
  
