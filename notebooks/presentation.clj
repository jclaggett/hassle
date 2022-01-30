;; # Transducer Nets
;; ## Presentation Outline
;; 1. Demo
;; 2. net transducers: how `net` works
;; 3. transducer nets: describing nets
;; 4. Kahn Process Networks
;; 5. Summary

;; ## 1. Demo
;; Start by defining an example transducer net and show how we can interact
;; with it.:
(def xfn-example
  (let [a (net/input :s1)
        b (net/input :s2)
        join-ab (net/join a b)
        msg (net/node (map (fn [[a b]] (str b " is " a " years old")))
                      join-ab)
        out (net/output :out msg)
        err (net/output :err #{(net/node (net/tag :a) a)
                               (net/node (net/tag :b) b)})]
    (net/net #{out err})))

;; Next, define an example sequence of input values for the above transducer
;; net:
(def xfn-inputs
  [[:s1 1] [:s2 "jim"] [:s1 2] [:s1 3] [:s2 "joe"] [:s2 "sue"]])

;; The example above is a transducer and can be used as one:
(into [] xfn-example xfn-inputs)

;; In addtion to being a transducer, transducer nets are also able to provide a graph describing their topology.
(xfn-example)

;; Same topology diagramed
(render-net xfn-example)

;; The exciting bit is that this network is entirely described with
;; transducers!

;; ## 2. net transducers: how `net` works
;; It's transducers all the way down and so we can learn about transducers made
;; by `net` by showing more primitive transducers that it uses.

;; ### 2.1 `final` transducer
;; The `final` transducer just appends a final value to the sequence of values.
;; It is really the transducer equivalent to `conj`. Also, closely related to
;; `wrap` found in https://github.com/cgrand/xforms
(def xf (final :done))
(sequence xf [])
(sequence xf [1 2 3])

;; ### 2.2 `tag` transducer
;; `tag` is built using `map` and `final` transducers and converts a sequence
;; of values into a sequence of 'tagged unions' (also known as variants). This definition is called tag' to avoid overriding the actual definition.

(defn tag' [k]
  (comp (map (partial vector k))
        (final [k])))
(sequence (tag' :foo) (range 4))

;; The sequence of tagged unions always ends with a special 'closing' tag. This final tag is useful when mutliple tagged union sequences are mixed together:

(interleave (sequence (tag' :a) (range 2))
            (sequence (tag' :b) (range 9 7 -1)))

;; ### 2.3 `detag` transducer
;; `detag` acts as the complement of `tag` and extracts the values from a
;; sequence of tagged untions. Non-tagged non-matching tagged values
;; are ignored. Also, the detagged sequnce stops when a 'closing' tag tag is found.

(sequence (comp (tag' :a)
                (detag :a))
          (range 3))

(sequence (detag :a)
          '([:a 1] [:b 2] [:a 3] 42 "bob" [:a 4] [:a] [:c 2] [:a "eh?"]))

;; ### 2.4 `multiplex` transducer
;; `multiplex` is a higher order transducer that applies each value to multiple
;; transducers. The results of all transducers are returned in a sequence.
;; `multiplex` is reduced when all transducers are reduced. This particualar
;; transducer is directly inspired by the `multiplex` found over at
;; https://github.com/cgrand/xforms

(sequence (multiplex [(take 2) (map -)]) (range 1 5))

(sequence (multiplex [(tag :a) (tag :b)]) (range 2))
(sequence (multiplex [(detag :a) (detag :b)]) [[:b "j"] [:b "w"] [:a 1]])

;; ### 2.5 `demultiplex` transducer
;; `demultiplex` acts the complement of multiplex in that multiple transducers
;; can share a single `demultiplex` transducer so that its state is shared.
;; _Warning_ This transducer is not safe to reuse across multiple uses of
;; transducers and so should only be used within the context of `multiplex`
;; transducers (or similar higher order transducers).

(let [common (demultiplex (take 2))
      xf-str (comp (map str) common)
      xf-neg (comp (map -) common)
      xf (multiplex [xf-str xf-neg])]
  (sequence xf [1 2 3 4])) 

;; ### 2.6 `net` transducer
;; Finally, we can use the above transducers to built a net of transducers into
;; a single high order transducer. This is what `net` does by taking a directed
;; acyclic graph of transducers and using `detag`, `tag`, `multiplex` and
;; `demultiplex` to compose the DAG of transducers into a single transducer.

;; TODO: draw two diagrams: one with network of xfs and same network expanded
;; with various xfs just described.

(clerk/table
  [[(let [i1 (net/input :a)
          i2 (net/input :b)
          n1 (net/node (map inc) #{i1 i2})
          n2 (net/node (map -) i2)
          o1 (net/output :c #{n1 n2})
          o2 (net/output :d n2)]
      (render-net (net/net #{o1 o2})))
    (-> (arr/create-graph)
        (arr/with-graph
          (render-node [:input :a] nil))
        arr/as-svg
        clerk/html)]])

;; ## 3. transducer nets: describing nets
;; ### 3.1 Primitives: `input`, `node`, `output`
;; ### 3.2 Embedding
;; ### 3.3 Joining

;; ## 4. Kahn Process Networks
;; ## 5. Summary
