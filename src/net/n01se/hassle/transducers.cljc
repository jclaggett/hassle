(ns net.n01se.hassle.transducers
  (:require [clojure.pprint :refer [pprint]]

            [net.n01se.hassle.net :refer [postwalk-net-map]]))

(defmacro reducer
  [[init & init-body]
   [step & step-body]
   [fini & fini-body]]
  (assert (= init 'init) (str "Expected init but got: " init))
  (assert (= step 'step) (str "Expected step but got: " step))
  (assert (= fini 'fini) (str "Expected fini but got: " fini))
  `(fn ~'reducer
     ~init-body
     ~step-body
     ~fini-body))
(defmacro init [rf] `(~rf))
(defmacro step [rf a v] `(~rf ~a ~v))
(defmacro fini [rf a] `(~rf ~a))

(defn final
  ([x xs] (sequence (final x) xs))
  ([x]
   (fn transducer [rf]
     (reducer
       (init [] (init []))
       (step [a v] (step rf a v))
       (fini [a] (fini rf (unreduced (step rf a x))))))))

(defn multiplex
  ([xfs xs] (sequence (multiplex xfs) xs))
  ([xfs]
   (fn transducer [rf]
     (let [rf-map (->> xfs
                       (map-indexed (fn [i xf] [i (xf rf)]))
                       (into (sorted-map)))
           *rf-map (volatile! rf-map)]
       (reducer
         (init [] (init rf))
         (step [a v]
               (let [a'' (reduce
                           (fn [a [k rf]]
                             (let [a' (step rf a v)]
                               (if (reduced? a')
                                 (do (vswap! *rf-map dissoc k)
                                     (fini rf (unreduced a')))
                                 a')))
                           a
                           @*rf-map)]
                 (if (empty? @*rf-map)
                   (reduced a'')
                   (unreduced a''))))
         (fini [a]
               (reduce (fn [a [_ rf]] (fini rf a))
                       a
                       @*rf-map)))))))

(defn demultiplex
  ([xf xs] (sequence (demultiplex xf) xs))
  ([xf]
   (let [*ref-count (volatile! 0)
         *cache (volatile! {})
         label (-> xf meta :label)]
     (fn transducer [rf]
       (vswap! *ref-count inc)
       (if (contains? @*cache :rf)
         (:rf @*cache)
         (let [rf' (xf rf)
               rf'' (reducer
                      (init []
                            (if (contains? @*cache :init)
                              (:init @*cache)
                              (:init (vswap! *cache assoc :init (init rf')))))
                      (step [a v]
                            (if (contains? @*cache :reduced)
                              (:reduced @*cache)
                              (let [a' (step rf' a v)]
                                (if (reduced? a')
                                  (:reduced (vswap! *cache assoc :reduced a'))
                                  a'))))
                      (fini [a]
                            (if (contains? @*cache :fini)
                              (:fini @*cache)
                              (if (or (zero? (vswap! *ref-count dec))
                                      (contains? @*cache :reduced))
                                (:fini (vswap! *cache assoc :fini (fini rf' a)))
                                a))))]
           (:rf (vswap! *cache assoc :rf rf''))))))))

(defn tag
  ([k xs] (sequence (tag k) xs))
  ([k] (comp (map (fn [x] [k x]))
             (final [k]))))

(defn detag
  ([k xs] (sequence (detag k) xs))
  ([k] (comp (filter #(= (first %) k))
             (take-while #(= (count %) 2))
             (map second))))

(defn match-tags
  ([xf-map xs] (sequence (match-tags xf-map) xs))
  ([xf-map] (multiplex (map (fn [[k xf]] (comp (detag k) xf)) xf-map))))

(defn get-root-nodes [net-map root]
  (->> (net-map root)
       (map (fn [[k v]] [k (net-map v)]))
       (into {})))

(defn netduce
  ([net-map xs] (sequence (netduce net-map) xs))
  ([net-map]
   (-> net-map
       (postwalk-net-map
         :outputs
         (fn [{:keys [args inputs outputs label] :as node} net-map]
           (let [label (if (nil? label) args label)
                 output-xfs (vary-meta (map net-map outputs) assoc :label label)
                 multiplex' (if (= (count outputs) 1) first multiplex)
                 demultiplex' (if (= (count inputs) 1) identity demultiplex)]
             (vary-meta
               (condp = (:type node)
                 :input (multiplex' output-xfs)
                 :node (demultiplex' (vary-meta
                                       (comp args (multiplex' output-xfs))
                                       assoc :label label))
                 :output (demultiplex' (vary-meta
                                         (tag args)
                                         assoc :label label)))
               assoc :label label))))
       (get-root-nodes :inputs)
       match-tags)))
