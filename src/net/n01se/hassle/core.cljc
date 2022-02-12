(ns net.n01se.hassle.core
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

                    (not (or (= tree-type :output) (nil? super-path)))
                    (-> (update-in (conj node-path :outputs) (fnil conj #{}) super-path)
                        (update-in (conj super-path :inputs) (fnil conj #{}) node-path))

                    true
                    (walk-trees sub-trees node-path))))
              net-map
              (normalize-trees trees)))]
    (walk-trees {} trees nil)))

(defn compact-net-map [net-map]
  (letfn [(compact-paths [paths] (map second paths))]
    (concat
      (map (fn [[k {:keys [outputs]}]]
             [k :outputs (compact-paths outputs)])
           (:input net-map))
      (map (fn [[k {:keys [outputs inputs]}]]
             [k :outputs (compact-paths outputs) :inputs (compact-paths inputs)])
           (:node net-map))
      (map (fn [[k {:keys [inputs]}]]
             [k :inputs (compact-paths inputs)])
           (:output net-map)))))

(defn postwalk-net-map [orig-net-map root-type update-fn]
  (let [kids (case root-type :input :outputs :inputs)
        root-paths (for [k (-> orig-net-map root-type keys)]
                     [root-type k])]

    (letfn [(update-node [net-map path]
              (update-in
                net-map path
                (fn [node]
                  (update-fn
                    path node
                    (for [kid-path (kids node)]
                      (get-in net-map kid-path))))))

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
        (visit-nodes root-paths))))) 

(defn assert-no-outputs [inputs]
  (assert (->> inputs normalize-trees (map first) (every? #{:input :node}))
          "Output nodes are not allowed as inputs")
  inputs)

;; Attempt 6? 7? at API
(defn match-tags
  ([xf-map xs] (sequence (match-tags xf-map) xs))
  ([xf-map] (t/multiplex (map (fn [[k xf]]
                                (comp (t/detag k)
                                      xf))
                              xf-map))))

(defn make-net-xf
  ([net-map xs] (sequence (make-net-xf net-map) xs))
  ([net-map]
   (-> net-map
       (postwalk-net-map
         :input
         (fn [[node-type node-id] {:keys [xf inputs outputs]}  output-xfs]
           (let [output-xfs' (if (empty? output-xfs) [identity] output-xfs)
                 multiplex' (if (= (count output-xfs') 1) first t/multiplex)
                 demultiplex' (if (= (count inputs) 1) identity t/demultiplex)]
             (condp = node-type
               :input (multiplex' output-xfs')
               :node (demultiplex' (comp xf (multiplex' output-xfs')))
               :output (demultiplex' (t/tag node-id))))))
       :input
       match-tags)))

(defn net
  ([net-tree xs] (sequence (net net-tree) xs))
  ([net-tree]
   (let [net-map (make-net-map net-tree)]
     (fn transducer
       ([] net-map)
       ([rf] ((make-net-xf net-map) rf))))))

(defn input [k] (list :input k #{}))
(defn node [xf inputs] (list :node (gensym 'n) (assert-no-outputs inputs) xf))
(defn output [k inputs] (list :output k (assert-no-outputs inputs)))

(defn postwalk [net-xf roots update-fn]
  (postwalk-net-map (net-xf) roots update-fn))

(defn embed [net-xf input-map]
  (-> net-xf
      (postwalk
        :output
        (fn [[node-type node-id] {xf :xf} input-xfs]
          (condp = node-type
            :input (input-map node-id)
            :node (node xf (set input-xfs))
            :output (set input-xfs))))
      :output))

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
  (->> inputs
       (map active)
       (map-indexed #(node (t/tag %1) %2))
       set
       (node (->> inputs
                  (map active?)
                  t/gate))))

(defn pr-net [net-xf]
  (compact-net-map (net-xf)))
