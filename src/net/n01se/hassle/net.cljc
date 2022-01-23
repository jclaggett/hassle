(ns net.n01se.hassle.net
  (:require [clojure.pprint :refer [pprint]]

            [net.n01se.hassle.transducers :as t]))

(defn debug
  ([x] (debug x x))
  ([msg x] (println "DEBUG:" msg) x))

;; implementation

(defn normalize-trees [trees]
  (if (set? trees)
    (mapcat normalize-trees trees)
    (if (nil? trees)
      (list)
      (list trees))))

(defn make-net-map
  [trees]
  (letfn [(walk-trees [net-map trees super-path]
            (reduce
              (fn [net-map [tree-type id sub-trees xf]]
                (let [node-path [tree-type id]]
                  (cond-> net-map
                    (not (nil? xf))
                    (assoc-in (conj node-path :xf) xf)

                    (not (or (= tree-type :outputs) (nil? super-path)))
                    (-> (update-in (conj node-path :outputs) (fnil conj #{}) super-path)
                        (update-in (conj super-path :inputs) (fnil conj #{}) node-path))

                    true
                    (walk-trees sub-trees node-path))))
              net-map
              (normalize-trees trees)))]
    (walk-trees {} trees nil)))

(defn postwalk-net-map [orig-net-map root update-fn]
  (let [kids (case root :inputs :outputs :outputs :inputs :none)]
    (letfn [(update-node [net-map path]
              (update-in net-map path
                         (fn [node]
                           (update-fn node path
                                      (map #(get-in net-map %)
                                           (kids node))))))

            (visit-node [net-map path]
              (if (-> net-map meta ::visited (contains? path))
                net-map
                (-> net-map
                    (vary-meta update ::visited conj path)
                    (visit-nodes (get-in net-map (conj path kids)))
                    (update-node path))))

            (visit-nodes [net-map paths]
              (reduce visit-node net-map paths))]

      (-> orig-net-map
        (vary-meta assoc ::visited #{})
        (visit-nodes (->> orig-net-map root keys (map (fn [k] [root k]))))))))

(defn compact-net-map [net-map]
  (letfn [(compact-paths [paths] (map second paths))]
    (concat
      (map (fn [[k {:keys [outputs]}]]
             [k :outputs (compact-paths outputs)])
           (:inputs net-map))
      (map (fn [[k {:keys [outputs inputs]}]]
             [k :outputs (compact-paths outputs) :inputs (compact-paths inputs)])
           (:nodes net-map))
      (map (fn [[k {:keys [inputs]}]]
             [k :inputs (compact-paths inputs)])
           (:outputs net-map)))))

(defn assert-no-outputs [inputs]
  (assert (->> inputs normalize-trees (map first) (every? #{:inputs :nodes}))
          "Output nodes are not allowed as inputs")
  inputs)

;; Attempt 6? 7? at API
(defn tag
  ([k xs] (sequence (tag k) xs))
  ([k] (comp (map (fn [x] [k x]))
             (t/final [k]))))

(defn detag
  ([k xs] (sequence (detag k) xs))
  ([k] (comp (filter #(= (first %) k))
             (take-while #(= (count %) 2))
             (map second))))

(defn match-tags
  ([xf-map xs] (sequence (match-tags xf-map) xs))
  ([xf-map] (t/multiplex (map (fn [[k xf]]
                                (comp (filter #(and (sequential? %)
                                                    (<= 1 (count %) 2)))
                                      (detag k)
                                      xf))
                              xf-map))))

(defn transduce-net
  ([net-map xs] (sequence (transduce-net net-map) xs))
  ([net-map]
   (-> net-map
       (postwalk-net-map
         :inputs
         (fn [{:keys [xf inputs outputs]} [node-type node-id] output-xfs]
           (let [output-xfs' (if (empty? output-xfs) [identity] output-xfs)
                 multiplex' (if (= (count output-xfs') 1) first t/multiplex)
                 demultiplex' (if (= (count inputs) 1) identity t/demultiplex)]
             (condp = node-type
               :inputs (multiplex' output-xfs')
               :nodes (demultiplex' (comp xf (multiplex' output-xfs')))
               :outputs (demultiplex' (tag node-id))))))
       :inputs
       match-tags)))

(defn net
  ([net-tree xs] (sequence (net net-tree) xs))
  ([net-tree]
   (let [net-map (make-net-map net-tree)
         net-xf (transduce-net net-map)]
     (fn transducer
       ([] net-map)
       ([rf] (net-xf rf))))))

(defn input [k] (list :inputs k #{}))
(defn node [xf inputs] (list :nodes (gensym 'n) (assert-no-outputs inputs) xf))
(defn output [k inputs] (list :outputs k (assert-no-outputs inputs)))

(defn embed [net-xf input-map]
  (-> (net-xf)
      (postwalk-net-map
        :outputs
        (fn [{xf :xf} [node-type node-id] input-xfs]
          (condp = node-type
            :inputs (input-map node-id)
            :nodes (node xf (set input-xfs))
            :outputs (set input-xfs))))
      :outputs))

(defrecord Passive [x]
  clojure.lang.IDeref
  (deref [_] x))

(defn passive? [x]
  (instance? Passive x))

(defn passive [x]
  (if (passive? x)
    x
    (Passive. x)))

(defn active? [x]
  (not (passive? x)))

(defn active [x]
  (if (active? x)
    x
    (deref x)))

(defn join [& inputs]
  (node (->> inputs
             (map active?)
             t/gate)
        (->> inputs
             (map active)
             (map-indexed #(node (tag %1) %2))
             set)))

(defn pr-net [net-xf]
  (compact-net-map (net-xf)))
