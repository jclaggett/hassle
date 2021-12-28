(ns net.n01se.hassle.net
  (:require [clojure.pprint :refer [pprint]]))

(defn debug [msg x] (println "DEBUG:" msg) x)

(defn make-net-map
  ([trees] (make-net-map {:inputs {}
                          :outputs {}}
                         trees
                         nil))

  ([net-map trees super-node-key]
   (reduce
     (fn [net-map [tree-type sub-trees args label :as tree]]
       (let [node-key (hash tree)]
         (cond-> net-map
           (not (contains? net-map node-key))
           (assoc node-key {:type tree-type
                            :args args
                            :label label
                            :inputs #{}
                            :outputs #{}})

           (= tree-type :input)
           (-> (assoc-in [:inputs args] node-key)
               (update node-key dissoc :inputs))

           (= tree-type :output)
           (-> (assoc-in [:outputs args] node-key)
               (update node-key dissoc :outputs))

           (not (nil? super-node-key))
           (-> (update-in [node-key :outputs] conj super-node-key)
               (update-in [super-node-key :inputs] conj node-key))

           true
           (make-net-map sub-trees node-key))))

     net-map
     (if (set? trees) trees #{trees}))))

(defn get-root-nodes [net-map root]
  (->> (net-map root)
       (map (fn [[k v]] [k (net-map v)]))
       (into {})))

(defn postwalk-net-map [orig-net-map root update-fn]
  (let [kids-fn (case root
                  :inputs :outputs
                  :outputs :inputs
                  :none)]
    (letfn [(update-node [net-map node-key]
              (update net-map node-key update-fn net-map))

            (visit-node [net-map node-key]
              (if (contains? (-> net-map meta ::visited) node-key)
                net-map
                (-> net-map
                    (vary-meta update ::visited conj node-key)
                    (visit-nodes (-> node-key net-map kids-fn))
                    (update-node node-key))))

            (visit-nodes [net-map node-keys]
              (reduce visit-node net-map node-keys))]

      (-> orig-net-map
        (vary-meta assoc ::visited #{})
        (visit-nodes (-> orig-net-map root vals))
        (get-root-nodes root)))))

(defn make-embed-fn [net-map]
  (vary-meta
    (fn embedder [input-map]
      (-> net-map
        (postwalk-net-map
          :outputs
          (fn [node net-map]
            node))))
    assoc ::net-map net-map))

(defmacro net [& args]
  (let [inputs (first args)
        typed-inputs (mapcat (fn [[k v]] [k (list `list :input #{} v)]) inputs)
        body (drop-last (rest args))
        outputs (last args)
        typed-outputs (map (fn [[k v]] (list `list :output v k)) outputs)]
    `^{::compose-net (fn [~inputs] (let [~@body] ~outputs))}
    (let [~@typed-inputs ~@body] (make-net-map #{~@typed-outputs}))))

;; Latest attempt at a decent API
(defn input [v] (list :input #{} v))
(def inputs (reify clojure.lang.IPersistentSet
              (get [_ v] (input v))))
(defn outputs [m] (make-net-map (set (map (fn [[k v]] (list :output v k)) m))))
(defn output [v k] (outputs {k v}))

(defn node
  ([xf] (output (node (input :in) xf) :out))
  ([in xf] (list :node in xf)))
