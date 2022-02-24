(ns net.n01se.hassle.transducers
  (:require [clojure.pprint :refer [pprint]]))

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
   (let [*ref-count (volatile! 0) ;; TODO: use an atom?
         *cache (volatile! {}) ;; TODO: use an atom?
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

(defn join-index-tags
  ([input-modes xs] (sequence (join-index-tags input-modes) xs))
  ([input-modes]
   (fn transducer [rf]
     (let [*inputs (volatile!
                     {:active (set (keep-indexed #(when %2 %1) input-modes))
                      :needed (set (range (count input-modes)))})
           *values (volatile! (vec (repeat (count input-modes) nil)))]
       (if (empty? (:active @*inputs))
         (reducer
           (init [] (init rf))
           (step [a v] (reduced a))
           (fini [a] (fini rf a)))
         (reducer
           (init [] (init rf))
           (step [a [i v :as tag]]
                 (if (= (count tag) 1) ;; i.e. this tag is ending.
                   (let [{:keys [active needed]}
                         (vswap! *inputs update :active disj i)]
                     (if (or (empty? active)
                             (needed i))
                       (reduced a)
                       a))
                   (let [{:keys [active needed]}
                         (vswap! *inputs update :needed disj i)
                         values (vswap! *values assoc i v)]
                     (if (and (active i)
                              (empty? needed))
                       (step rf a values)
                       a))))
           (fini [a] (fini rf a))))))))

(defn tag
  ([k xs] (sequence (tag k) xs))
  ([k] (comp (map (fn [x] [k x]))
             (final [k]))))

(defn detag
  ([k xs] (sequence (detag k) xs))
  ([k] (comp (filter #(and (sequential? %)
                           (not (empty? %))
                           (= (first %) k)))
             (take-while #(= (count %) 2))
             (map second))))

