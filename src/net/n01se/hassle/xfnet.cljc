(ns net.n01se.hassle.xfnet
  (:require [net.n01se.hassle.net :as net])
  (:require [net.n01se.hassle.transducers :as t]))

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
       (net/postwalk
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
   (let [net-map (net/normalize-net label net-tree)]
     ^:xfn
     (fn transducer
       ([] net-map)
       ([rf] ((make-net-xf transducer) rf))))))

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
  ;; The nodes defined here are mislabeled for readability. Short term fix.
  ;; Longer term, join should be represented as a subnet.
  (let [active-inputs? (mapv active? inputs)]
    (net/node*
      'join
      (t/join-index-tags active-inputs?)
      (->> inputs
           (map active)
           (map-indexed
             #(net/node*
                (if (active-inputs? %1) 'active 'passive)
                (t/tag %1)
                %2))
           set))))

;; Re-export these for now
(def input net/input)
(def output net/output)
(def node* net/node*)

(defn embed [xfn input-map]
  (net/embed (xfn) input-map))

(defn postwalk [xfn roots update-fn]
  (net/postwalk (xfn) roots update-fn))

(defn pr-xfn [xfn]
  (net/compact-net-map (xfn)))
