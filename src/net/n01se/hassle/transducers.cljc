(ns net.n01se.hassle.transducers
  (:require [clojure.pprint :refer [pprint]]

            [net.n01se.hassle.net :as n :refer [postwalk-net-map]]))

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
  ([xf-map] (multiplex (map (fn [[k xf]]
                              (comp (filter #(and (sequential? %)
                                                  (<= 1 (count %) 2)))
                                    (detag k)
                                    xf))
                            xf-map))))

(defn vary-rf-meta [xf & args]
  (fn transducer [rf]
    (apply vary-meta (xf rf) args)))

(defn netduce
  ([net-map xs] (sequence (netduce net-map) xs))
  ([net-map]
   (-> net-map
       (postwalk-net-map
         :inputs
         (fn [{:keys [xf inputs outputs]} [node-type node-id] output-xfs]
           (let [output-xfs' (if (empty? output-xfs) [identity] output-xfs)
                 multiplex' (if (= (count output-xfs') 1) first multiplex)
                 demultiplex' (if (= (count inputs) 1) identity demultiplex)]
             (condp = node-type
               :inputs (multiplex' output-xfs')
               :nodes (demultiplex' (comp xf (multiplex' output-xfs')))
               :outputs (demultiplex' (tag node-id))))))
       :inputs
       match-tags
       (vary-meta assoc ::net net-map)
       (vary-rf-meta assoc ::net net-map))))

(defn start-net [net-map]
  {:log []
   :reduced? false
   :reducer ((netduce net-map) (completing conj))})

(defn next-tag [state tag]
  (if (:reduced? state)
    state
    (let [result ((:reducer state) [] tag)]
      (-> state
          (assoc :reduced? (reduced? result))
          (update :log conj [tag (unreduced result)])))))

;; Newest, take on an API. Take 5?
(defn input
  ([k xs] (sequence (input k) xs))
  ([k]
   (let [net-tree (n/input k)]
     (vary-meta
       (fn transducer [rf]
         ((-> net-tree
              n/make-net-map
              netduce)
          rf))
       assoc ::net-tree net-tree))))

(defn get-input-trees [input-xfs]
  (let [input-trees (->> input-xfs
                         n/get-trees
                         (map (comp ::net-tree meta)))]
    (assert (not (empty? input-trees)) "Must specify at least one input.")
    (assert (not-any? nil? input-trees) "All inputs must be net transducers.")
    (set input-trees)))

(defn node
  ([xf inputs xs] (sequence (node xf inputs) xs))
  ([xf input-xfs]
   (let [input-trees (get-input-trees input-xfs)
         net-tree (n/node xf input-trees)]
     (vary-meta
       (fn transducer [rf]
         ((-> net-tree
              n/make-net-map
              netduce)
          rf))
       assoc ::net-tree net-tree))))

(defn outputs
  ([input-xf-map xs] (sequence (outputs input-xf-map) xs))
  ([input-xf-map]
   (let [input-tree-map (->> input-xf-map
                             (map (fn [[k v]] [k (get-input-trees v)]))
                             (into {}))
         net-tree (n/outputs input-tree-map)]
     (vary-meta
       (fn transducer [rf]
         ((-> net-tree
              n/make-net-map
              netduce)
          rf))
       assoc ::net-tree net-tree))))
