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
