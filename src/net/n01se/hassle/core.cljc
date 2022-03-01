(ns net.n01se.hassle.core
  (:require [clojure.pprint :refer [pprint]]

            [net.n01se.hassle.transducers :as t]))

(defn debug
  ([x] (debug x x))
  ([msg x] (println "DEBUG:" msg) x))

;; implementation

(defn normalize-inputs [inputs]
  (if (set? inputs)
    (mapcat normalize-inputs inputs)
    (if (nil? inputs)
      (list)
      (list inputs))))

(defn normalize-net
  [label trees]
  (letfn [(walk-trees [net-map trees super-path]
            (reduce
              (fn [net-map [tree-type id sub-trees xf label]]
                (let [node-path [tree-type id]]
                  (cond-> net-map
                    (not (nil? xf))
                    (assoc-in (conj node-path :xf) xf)

                    (not (nil? label))
                    (assoc-in (conj node-path :label) label)

                    (not (or (= tree-type :output) (nil? super-path)))
                    (-> (update-in (conj node-path :outputs) (fnil conj #{}) super-path)
                        (update-in (conj super-path :inputs) (fnil conj #{}) node-path))

                    true
                    (walk-trees sub-trees node-path))))
              net-map
              (normalize-inputs trees)))]
    (walk-trees {:label label} trees nil)))

(defn postwalk [net-xf roots update-fn]
  (let [orig-net-map (net-xf)
        kids (case roots :input :outputs :inputs)
        root-paths (for [k (-> orig-net-map roots keys)]
                     [roots k])]

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
  (assert (->> inputs normalize-inputs (map first) (every? #{:input :node}))
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
  ([net-xf xs] (sequence (make-net-xf net-xf) xs))
  ([net-xf]
   (-> net-xf
       (postwalk
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
  ([label net-tree xs] (sequence (net net-tree) xs))
  ([label net-tree]
   (let [net-map (normalize-net label net-tree)]
     ^:xfn
     (fn transducer
       ([] net-map)
       ([rf] ((make-net-xf transducer) rf))))))

(defn input [k] (list :input k #{}))
(defn output [k inputs] (list :output k (assert-no-outputs inputs)))

(defn node* [label xf inputs]
  (list :node
        (gensym 'n)
        (assert-no-outputs inputs)
        xf
        label))

(defmacro node [xf inputs]
  (let [label (if (sequential? xf)
                (if (symbol? (first xf))
                  (if (= (count xf) 1)
                    (str (first xf))
                    (if (sequential? (second xf))
                      (str (first xf) " …")
                      (if (> (count xf) 2)
                        (str (first xf) " " (second xf) " …")
                        (str (first xf) " " (second xf))))))
                (str xf))]
    `(node* '~label ~xf ~inputs)))

(defn embed [net-xf input-map]
  (-> net-xf
      (postwalk
        :output
        (fn [[node-type node-id] {xf :xf label :label} input-xfs]
          (condp = node-type
            :input (input-map node-id)
            :node (node* label xf (set input-xfs))
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

(defn join [inputs]
  ;; This let is only to print nicer results from node macro
  (let [join-tags (t/join-index-tags (map active? inputs))
        tag t/tag]
    (node join-tags
          (->> inputs
               (map active)
               (map-indexed (fn [i v] (node (tag i) v)))
               set))))

;; printing/debugging
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

(defn pr-net [net-xf]
  (compact-net-map (net-xf)))
