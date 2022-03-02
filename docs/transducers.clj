;; # Review of `net` transducers
;; It's transducers all the way down and so we can learn about the `net`
;; transducer by showing the more primitive transducers it uses.

;; This notebook takes a bottom up approach to describing these transducers
;; starting with the most primitive and simple transducers and ending with the
;; `net` transducer.

^{:nextjournal.clerk/visibility #{:hide}
  :nextjournal.clerk/viewer :hide-result}
(require '[net.n01se.hassle.transducers
           :refer [final tag detag multiplex demultiplex]]
         '[net.n01se.hassle.core
           :refer [net input node output]])

;; ## Transducer Composition Tree
;; This diagram shows the 'composition tree' of transducers used to define `net` (blue are
;; transducers added in hassle):
^{:nextjournal.clerk/visibility #{:hide}}
(letfn [(vertex! [label & opts] (apply arr/insert-vertex! label opts))
        (edge! [from to & opts] (apply arr/insert-edge! from to :stroke-width 1 opts))]
  (-> (arr/create-graph)
      (arr/with-graph
        (let [v-map (vertex! "map")
              v-take-while (vertex! "take-while")
              v-filter (vertex! "filter")
              v-final (vertex! "final" :fill-color "lightblue")
              v-tag (vertex! "tag" :fill-color "lightblue")
              v-detag (vertex! "detag" :fill-color "lightblue")
              v-multiplex (vertex! "multiplex" :fill-color "lightblue")
              v-demultiplex (vertex! "demultiplex" :fill-color "lightblue")
              v-net (vertex! "net" :fill-color "lightblue")]
          (edge! v-filter v-detag)
          (edge! v-take-while v-detag)
          (edge! v-map v-detag)
          (edge! v-final v-tag)
          (edge! v-map v-tag)
          (edge! v-tag v-net)
          (edge! v-detag v-net)
          (edge! v-multiplex v-net)
          (edge! v-demultiplex v-net)))

    arr/as-svg
    clerk/html))

;; ## Built-in Transducers
;; Several of the most primitve transducers in the tree come standard with
;; Clojure. These are briefly described here. Also, this is a chance for those
;; new to transducers to at least see how they are used.

;; `map` transducers apply a function to each item in a sequence.
(sequence (map #(* % %))
          (range 6))

;; `take-while` transducers stops once its predicate returns false.
(sequence (take-while number?)
          [1 3.14 5 6 "foo" -2 0])

;; `filter` transducers only allows specific values to pass.
(sequence (filter odd?)
          (range 9))

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

;; ## `tag` Transducer
;; `tag` is built using `map` and `final` transducers and converts a sequence
;; of values into a sequence of 'tagged' values (also known as tagged unions or
;; variants).

;; The definition of tag is (named as tag' to avoid the original):
^{:nextjournal.clerk/viewer :hide-result}
(defn tag' [k]
  (comp (map (partial vector k))
        (final [k])))

(sequence (tag' :foo) (range 4))

;; The sequence of tagged unions always ends with a special final tag. This
;; final tag is useful when mutliple tagged union sequences are mixed together:

(interleave (sequence (tag' :a) (range 2))
            (sequence (tag' :b) (range 9 7 -1)))

;; ## `detag` Transducer
;; `detag` acts as the complement of `tag` by extracting the original sequence
;; of values from the tagged sequence. Non-tagged non-matching tagged values
;; are ignored. Also, the detagged sequence stops when a final tag is found.

;; The definition of detag is (renamed as detag' to avoid the original):
^{:nextjournal.clerk/viewer :hide-result}
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

;; ## `multiplex` Transducer
;; `multiplex` is a higher order transducer that applies each value to multiple
;; transducers. The results of all transducers are returned in a sequence.
;; `multiplex` is reduced when all transducers are reduced. This particular
;; transducer is directly inspired by the `multiplex` transducer found over at
;; https://github.com/cgrand/xforms.
(sequence (multiplex [(take 2) (map -)]) (range 1 5))
(sequence (multiplex [(tag' :a) (tag' :b)]) (range 2))
(sequence (multiplex [(detag' :a) (detag' :b)]) [[:b "j"] [:b "w"] [:a 1]])

;; ## `demultiplex` Transducer
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

;; ## `net` Transducer
;; Finally, we come to the `net` transducer that combines all of the above
;; transducers. Roughly speaking they are combined as follows: `detag` ->
;; `multiplex` -> ... -> `demultiplex` -> `tag`.

;; As a simple example demonstrating the above transducer chain:
(sequence (->> (input :foo)

               (node (map inc))
               (node (map str))

               (output :bar)

               (net 'simple-chain))
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

;; This means visualization tools, like the one used in this notebook, may be
;; used.

;; To learn more on representing transducer nets for the `net` transducer,
;; refer to the ajoining notebook
