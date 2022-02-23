;; # Review of `net` transducers
;; It's transducers all the way down and so we can learn about the `net`
;; transducer by showing the more primitive transducers it uses.

;; This notebook takes a bottom up approach to describing these transducers
;; starting with the most primitive and simple transducers and ending with the `net` transducer.

(do
  (require '[net.n01se.hassle.transducers
             :refer [final tag detag multiplex demultiplex]])
  (require '[net.n01se.hassle.core
             :refer [net input node output]]))

;; ## Transducer Composition Tree
;; This diagram shows the 'composition tree' of transducers used to define `net` (blue are
;; transducers added):
(render-xf-compose-tree)

;; ## built-in transducers
;; Several of the most primitve transducers in the tree come standard with
;; Clojure. These are briefly described here. Also, this is a chance for those
;; new to transducers to at least see how they are used.

;; `map` transducers apply a function to each item in a sequence.
(sequence (map #(* % %))
          (range 6))

;; `take-while` transducers stops once its predicate returns false.
(sequence (take-while number?)
          [1 3.14 5 6 "foo" -2 0])

;; ## `final` transducer
;; The `final` transducer just appends a final value to the sequence of values.
;; It is really the transducer equivalent to `conj`. Also, closely related to
;; `wrap` found in https://github.com/cgrand/xforms
(sequence (final :done)
          [])
(sequence (final :done)
          [:a :b :c])
(sequence (final 4)
          [1 2 3])

;; ## `tag` transducer
;; `tag` is built using `map` and `final` transducers and converts a sequence
;; of values into a sequence of 'tagged' values (also known as tagged unions or
;; variants).

;; The definition of tag is (named as tag' to avoid the original):
(defn tag' [k]
  (comp (map (partial vector k))
        (final [k])))

(sequence (tag' :foo) (range 4))

;; The sequence of tagged unions always ends with a special final tag. This
;; final tag is useful when mutliple tagged union sequences are mixed together:

(interleave (sequence (tag' :a) (range 2))
            (sequence (tag' :b) (range 9 7 -1)))

;; ## `detag` transducer
;; `detag` acts as the complement of `tag` by extracting the original sequence
;; of values from the tagged sequence. Non-tagged non-matching tagged values
;; are ignored. Also, the detagged sequence stops when a final tag is found.

;; The definition of detag is (renamed as detag' to avoid the original):
(defn detag' [k]
  (comp (filter #(and (sequential? %)
                      (not (empty? %))
                      (= (first %) k)))
        (take-while #(= (count %) 2))
        (map second)))

(sequence (detag :a)
          '([:a 1] [:b 2] [:a 3] 42 "bob" [:a 4] [:a] [:c 2] [:a "eh?"]))

(sequence (comp (tag' :a)
                (detag' :a))
          (range 3))

;; ## `multiplex` transducer
;; `multiplex` is a higher order transducer that applies each value to multiple
;; transducers. The results of all transducers are returned in a sequence.
;; `multiplex` is reduced when all transducers are reduced. This particular
;; transducer is directly inspired by the `multiplex` transducer found over at
;; https://github.com/cgrand/xforms.
(sequence (multiplex [(take 2) (map -)]) (range 1 5))
(sequence (multiplex [(tag' :a) (tag' :b)]) (range 2))
(sequence (multiplex [(detag' :a) (detag' :b)]) [[:b "j"] [:b "w"] [:a 1]])

;; ## `demultiplex` transducer
;; `demultiplex` acts the complement of multiplex in that multiple transducers
;; can share a single `demultiplex` transducer so that its state is shared.

;; **Warning**: transducers that do share a common `demultiplex` must in turn
;; be composed using a common transducer such as `multiplex`. If you try to use
;; `demultiplex` across different runs, the results will be 'surprising'.

(let [a (demultiplex (take 3))
      b (comp (map str) a) ;; not safe to run a alone!
      c (comp (map -) a)   ;; not safe to run b alone!
      d (multiplex [b c])]  ;; safe to run a and b together
  (sequence d [1 2 3 4]))

;; ## `net` transducer
;; Finally, we come to the `net` transducer that combines all of the above
;; transducers. Roughly speaking they are combined as follows: `detag` ->
;; `multiplex` -> ... -> `demultiplex` -> `tag`.

;; As a simple example demonstrating the above transducer chain:
(sequence (->> (input :foo)
               (node (map inc))
               (node (map str))
               (output :bar)
               (net 'simple))
          [[:foo 1] [:foo 23] [:foo]])

;; By using `multiplex` and `demultiplex`, the `net` transducer is able to
;; represent arbitrary DAGs of transducers.

;; Define a transducer net:
(def multi-io-example
  (let [i1 (input :a)
        i2 (input :b)
        n1 (node (map inc) #{i1 i2})
        n2 (node (map -) i2)
        o1 (output :c #{n1 n2})
        o2 (output :d n2)]
    (net 'multi-io-example #{o1 o2})))

;; Apply it to a sequence of inputs
(sequence multi-io-example [[:a 1] [:b 2] [:b] [:a 3] [:a]])

;; Once a transducer net is defined, the net's specification may be accessed
;; via zero arg invocation of the transducer net:
(multi-io-example)

;; This means visualization tools may be used:

(render-net multi-io-example)

;; To learn more on representing transducer nets for the `net` transducer, refer to the ajoining notebook
