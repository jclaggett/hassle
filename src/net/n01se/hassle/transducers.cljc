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


(defn net-transducer [net-type net-tree]
  (vary-meta
    (fn transducer [rf]
      ((-> net-tree n/make-net-map netduce) rf))
    assoc ::net-type net-type ::net-tree net-tree))

(defn get-trees [xfs]
  (->> xfs
       n/get-input-trees
       (map (comp ::net-tree meta))
       set))

(defn get-input-trees [xfs]
  (assert (->> xfs
               n/get-input-trees
               (map (comp ::net-type meta))
               (every? #{:inputs :nodes}))
          (str "Error: bad input value: '" xfs "'. "
               "Must specify net-transducers of the type(s): "
               #{:inputs :nodes}))
  (get-trees xfs))

;; Newest, take on an API. Take 5?
(defn net
  ([xfs xs] (sequence (net xfs) xs))
  ([xfs]
   (let [trees (get-trees xfs)]
     (net-transducer :outputs trees))))

(defn input
  ([k xs] (sequence (input k) xs))
  ([k] (net-transducer :inputs (n/input k))))

(defn node
  ([xf input-xfs xs] (sequence (node xf input-xfs) xs))
  ([xf input-xfs]
   (let [input-trees (get-input-trees input-xfs)]
     (net-transducer :nodes (n/node xf input-trees)))))

(defn output
  ([k input-xfs xs] (sequence (output k input-xfs) xs))
  ([k input-xfs]
   (let [input-trees (get-input-trees input-xfs)]
     (net-transducer :outputs (n/output k input-trees)))))

(defn embed
  ([xf input-xf-map xs] (sequence (embed xf input-xf-map) xs))
  ([xf input-xf-map]
   (let [{::keys [net-type net-tree]} (meta xf)]
     (assert (#{:outputs} net-type)
             (str "Error: bad embed value: '" xf "'. Must specify `output` net transducer."))
     (-> net-tree
         n/make-net-map
         (n/postwalk-net-map
           :outputs
           (fn [{xf :xf} [node-type node-id] input-xfs]
             (condp = node-type
               :inputs (input-xf-map node-id)
               :nodes (node xf (set input-xfs))
               :outputs (set input-xfs))))
         :outputs))))

;; Building upwards from the above primitives

(defn gate
  ([input-modes xs] (sequence (gate input-modes) xs))
  ([input-modes]
   (fn transducer [rf]
     (let [*active-inputs (volatile! (set
                                       (keep-indexed #(when %2 %1)
                                                     input-modes)))
           *received-inputs (volatile! #{})
           *input-values (volatile! (vec (repeat (count input-modes) nil)))]
       (if (empty? @*active-inputs)
         ;; if there are no active inputs ever, this transducer is trivial.
         (reducer
           (init [] (init rf))
           (step [a v] (reduced a))
           (fini [a] (fini rf a)))
         (reducer
           (init [] (init rf))
           (step [a [i v :as couple]]
                 (if (= (count couple) 1) ;; i.e. this stream is ending
                   (let [active-inputs (vswap! *active-inputs disj i)
                         received-inputs @*received-inputs]
                     (if (or (empty? active-inputs)
                             (and (< (count received-inputs) (count input-modes))
                                  (not (contains? received-inputs i))))
                       (reduced a)
                       a))
                   (let [received-inputs (vswap! *received-inputs conj i)
                         input-values (vswap! *input-values assoc i v)]
                     (if (and (= (count received-inputs) (count input-modes))
                              (nth input-modes i))
                       (step rf a input-values)
                       a))))
           (fini [a] (fini rf a))))))))

(defn passively [x]
  (vary-meta x assoc ::passive true))

(defn join [& inputs]
  (let [input-modes (map #(-> % meta (::passive false) not) inputs)]
    (pprint {:join input-modes})
    (->> inputs
         (map-indexed (fn [i input] (node (tag i) input)))
         set
         (node (gate input-modes)))))

(defn pr-net [xfs]
  (-> xfs
      get-trees
      n/make-net-map
      n/compact-net-map))
