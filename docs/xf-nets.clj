;; # Transducer Nets
;; This notebook walks through the core library of functions used to define
;; transducer nets. It starts with the primitives: `net`, `input`, `node`, and `output`
;; and then covers the more advanced functions `join` and `embed`.

^{:nextjournal.clerk/visibility #{:hide}
  :nextjournal.clerk/viewer :hide-result}
(require '[net.n01se.hassle.xfnet
           :refer [net input node output join passive embed]])

;; ## `net` Function
;; `net` is always found at the end of a transducer net specification and it
;; converts that specification into a transducer. Also, `net` takes a label to
;; be associated with the net specification for documentation purposes.

;; The smallest transducer net definable is an empty one:
(def empty-net (net 'empty-net nil))

;; This is a proper transducer function that just ignores all input:
(sequence empty-net [1 2 3]) ;;

;; In addition to being a transducer, net functions also may be called with
;; zero arguments and they return a normalized representation of the net:

(empty-net)

;; ## `input` & `output` Functions
;; The next smallest transducer net has a single input and a single output
;; connected to each other:

(def net-2
  (let [in (input :in)
        out (output :out in)]
    (net 'net-2 out)))

;; Looking at this net's normalized representation is more interesting than
;; `empty-net`:
(net-2)

;; ### Connectivity Properties
;; For a transducer net to be valid, there are two connectivity properties that
;; must be true:
;; 1. Each input is referenced by at least one output.
;; 2. Each output references at least one input.

;; While it is possible to describe a net with unconnected inputs or outputs,
;; it won't work and may lose track of unconnected elements.

;; ### Tagged Unions for IO
;; To use transducer nets, they expect a sequence of tagged unions or variants
;; as input and produce a sequence of tagged unions. A tagged union is just a
;; vector pair like: `[tag value]`. Additionally, a 'final' tagged union that
;; contains just the tag and no value is used to indicate the end of that
;; sequence of tagged unions: `[tag]`.

;; In the case of `net-2`, it expects tagged unions with the tag `:in`
;; and will produce a sequence of tagged unions with the tag `:out`.

;; Here we feed `net-2` the following sequence of various values:

(sequence net-2 [[:in "hello"]      ;; ✓ correct tag
                 [:wrong "goodbye"] ;; 🗴 wrong tag
                 42                 ;; 🗴 not a tagged union
                 [:in "world"]      ;; ✓ correct tag
                 [:in]              ;; ✓ final tag
                 [:in "hello?"]     ;; 🗴 after final tag
                 nil])

;; ### Multiple Inputs
;; In the above example, the output was connected to a single input. To
;; connect an output to multiple inputs, a standard Clojure Set is used as in
;; this example:

(let [in-1 (input :in-1)
      in-2 (input :in-2)

      out (output :out #{in-1 in-2})] ;; Use #{} to connect multiple inputs
  (net 'net-3 out))

;; ### Multiple Outputs
;; Of course it is also possible to have multiple outputs connected to a single
;; input:

(let [in (input :in)

      out-1 (output :out-1 in)
      out-2 (output :out-2 in)]
  (net 'net-4 #{out-1 out-2})) ;; now net uses #{} to handle multiple outputs

;; ### Multiple Inputs & Outputs
;; Just for completeness, a transducer net with two inputs and two outputs:

(let [a (input :a)
      b (input :b)

      c (output :c a)
      d (output :d #{a b})]
   (net 'net-5 #{c d}))

;; ## `node` Function
;; The body of transducer nets is defined by calls to the `node` function. Each
;; call specifies the transducer for that node in the net. Each node is
;; considered to be both an input and output in terms of the two previously
;; mentioned connectivity properties. In other words, each node references at
;; least one input and is referenced by at least one output.

;; ### Single Node Example
;; Here is a simple transducer net with a single node:
(def net-6
  (let [in (input :in)

        n1 (node (map inc) in)

        out (output :out n1)]
    (net 'net-6 out)))

(sequence net-6 [[:in 1] [:in 2] [:in 3]])

;; ### Multi-Node Example
;; Here is a more complex transducer with multiple nodes and multiple inputs:
(def net-7
  (let [untrusted (input :untrusted)
        trusted (input :trusted)

        numbers (node (take-while number?) untrusted)
        odd-numbers (node (filter odd?) numbers)
        even-numbers (node (filter even?) numbers)
        region1-numbers (node (map inc) odd-numbers)
        region2-numbers (node identity even-numbers)

        region1 (output :region1 #{region1-numbers trusted})
        region2 (output :region2 #{region2-numbers trusted})]
    (net 'net-7 #{region1 region2})))

(sequence net-7 [[:untrusted 234]
                 [:untrusted 237]
                 [:untrusted 238]
                 [:trusted -6]
                 [:untrusted :wat]
                 [:untrusted 239]
                 [:trusted -8]])

;; ## `join` Function
;; So far all connections have been independent. Values flowing into an output
;; from one connection is independent from values flowing in from other
;; connections.  `join` synchronizes values flowing across multiple
;; connections. First, `join` waits for at least one value to arrive on each
;; connection and, from that point on, will produce a vector containing the
;; latest value for each connection when a new value is sent on any connection.

(def net-8
  (let [in-1 (input :in-1)
        in-2 (input :in-2)

        a (join [in-1 in-2])

        o (output :out a)]
    (net 'net-8 o)))

(sequence net-8 [[:in-1 'a]
                 [:in-1 'b]
                 [:in-2 1]
                 [:in-1 'c]
                 [:in-2 2]])

;; ### `passive` Joins
;; In the previous example, a value arriving on either connection caused `join`
;; to produce a new vector. When calling `join`, some of the inputs may be
;; wrapped using `passive`. Values flowing through passive connections will not
;; cause `join` to produce a vector. Marking all inputs as `passive` will cause
;; `join` to never emit a value.

(def net-9
  (let [in-1 (input :in-1)
        in-2 (input :in-2)

        a (join [(passive in-1) in-2])

        o (output :out a)]
    (net 'net-9 o)))

(sequence net-9 [[:in-1 'a]
                 [:in-1 'b]
                 [:in-2 1]
                 [:in-1 'c]
                 [:in-2 2]])

;; ## `embed` Function
;; The last function provided in the core API provides a way to define new
;; transducer nets using previously defined nets. `embed` takes a map
;; associating inputs to the input tags found in the embedded net and it
;; returns a map of output tags to outputs. Once `embed` is complete, the
;; inputs and outputs of the embedded net are replaced with direct connections
;; to nodes in the new net.

(def net-10
  (let [in-a (input :in-a)
        in-b (input :in-b)

        {a :region1
         b :region2} (embed net-7
                            {:trusted in-a
                             :untrusted in-b})

        c (node (map str) b)

        out-a (output :out-a a)
        out-b (output :out-b b)
        debug (output :debug c)]

    (net 'net-10 #{out-a out-b debug})))
