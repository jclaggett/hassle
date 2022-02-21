# Transducer Nets
Using transducers to describe computational graphs.

## Outline

1. Intro and Terms
2. Tutorial on making transducer nets
3. Digging into `net` transducer

# netducer and other transducers

## transducer 1: final
```clojure
  ;=> (v1 v2 v3)
(final :last)
  ;=> (v1 v2 v3 :last)
```

### transducer 1: discussion
Like conj. Clojure _should_ support a 2-arity call to conj
(and cons!) that returns a transducer.

See also: cgrand's xform/wrap

### transducer 1: implementation
```clojure
;; TODO
```

## transducer 2: tag
```clojure
  ;=> (0 1 2)
(tag :a)
  ;=> ([:a 0] [:a 1] [:a 2] [:a])
```

## transducer 3: detag
The benefit of tagged values is they can be merged with other tagged values and
pulled apart later. They serialize nicely.

```clojure
  ;=> ([:a 0] [:b -1] [:b -2] [:a 1] [:b] [:a 2] [:a])
(detag :a)
  ;=> (0 1 2)

  ;=> ([:a 0] [:b -1] [:b -2] [:a 1] [:b] [:a 2] [:a])
(detag :b)
  ;=> (-1 -2)
```

## transducers 2 & 3: tag & detag implementation
```clojure
;; TODO
```

## transducer 4: multiplex
```clojure
  ;=> (0 1 2)
(multiplex [identity identity])
  ;=> (0 0 1 1 2 2)

  ;=> (0 1 2)
(multiplex [(take 2) (map -)])
  ;=> #_(0 0 1 -1 -2)

  ;=> (0 1 2)
(multiplex [(comp (take 2) (tag :a))
            (comp (map -) (tag :b))])
  ;=> #_([:a 0] [:b 0] [:a 1] [:a] [:b -1] [:b -2] [:b])
```

### transducer 4: discussion
Roughly like juxt.
See also: cgrand's xform/multiplex

## transducer 5: demultiplex
```clojure
  ;=> (0 1 2)
(let [shared (demultiplex (comp (take 2) (tag :out)))]
  (multiplex [(comp (detag :a) shared) (comp (detag :b) shared)]))
  ;=> (0 0 1 1 2 2)
```

### transducer 5: discussion
Demuliplex with multiplex allows us to define DAGs of transducer functions and
represent those DAGs as a single transducer function.

## transducer 6: netducer
```clojure
(def input '([:a 0] [:b 0] [:a 1] [:a] [:b -1] [:b -2] [:b]))
(match-tags [[:a identity] [:b identity]] input)
  #_(0 0 1 1 2 2)
```
## transducer 6: netducer

```clojure
;; TODO
```
