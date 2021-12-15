# dagducers and other transducers

## transducer 1: final
```clojure
(final 3 [0 1 2])
  ;=> (0 1 2 3)
(sequence (final 4) (range 3))
  ;=> (0 1 2 3)
```

### transducer 1: discussion
Like conj. Clojure _should_ support a 2-arity call to conj
(and cons!) that returns a transducer.

See also: cgrand's xform/wrap

### transducer 1: implementation
```clojure
;; TODO
```

## transducer 2: vstream
```clojure
(vstream :a [0 1 2])
  #_([:a 0] [:a 1] [:a 2] [:a])
```

### transducer 2: discussion
The benefit of variant streams is they _can_ be crossed! And also separated. In
short, they serialize nicely.

```clojure
([:a 0] [:b 1] [:b 0] [:a 1] [:b] [:a 2] [:a])
```

### transducer 2: implementation
```clojure
;; TODO
```

## transducer 3: multiplex
```clojure
(multiplex [identity identity] (range 3))
  #_(0 0 1 1 2 2)
(multiplex [(take 2) (map -)] (range 3))
  #_(0 0 1 -1 -2)
(multiplex [(comp (take 2) (vstream :a))
            (comp (map -) (vstream :b))]
           (range 3))
  #_([:a 0] [:b 0] [:a 1] [:a] [:b -1] [:b -2] [:b])
```

### transducer 3: discussion
Roughly like juxt.
See also: cgrand's xform/multiplex

## transducer 4: switch-vstreams
```clojure
(def input '([:a 0] [:b 0] [:a 1] [:a] [:b -1] [:b -2] [:b]))
(switch-vstreams [[:a identity] [:b identity]] input)
  #_(0 0 1 1 2 2)
```

### transducer 4: discussion
Could use a better name...

### transducer 4: implemenation
```clojure
;; TODO
```

## transducer 5: demultiplex
```clojure
(demultiplex (comp (take 2) (vstream :out)) (range 3))
  #_([:out 0] [:out 1] [:out])

(def input '([:a 0] [:b 0] [:a 1] [:a] [:b -1] [:b -2] [:b]))
(def shared (demultiplex (comp (take 2) (vstream :out)))
(switch-vstreams {:a (comp (map inc) shared)
                  :b (comp (map dec) shared)}
                 input)
  #_([:out 1] [:out -1] [:out])
```

### transducer 5: discussion
Demuliplex with multiplex (and switch-vstreams) allows us to define DAGs of
transducer functions and represent those DAGs as a single transducer function.


## transducer 6: dagducer

```clojure
;; TODO
```
