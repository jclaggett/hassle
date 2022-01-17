# Hassle

An experimental reactive functional library for Clojure. The primary goal is to
demonstrate the ability to express programs as a DAG of functions over streams
of events.

```clojure
#_procedural-imperative
(defn main []
  (println "Hello World!"))

#_functional-declarative
(def main
  (->> (h/input :init)
       (h/node (map #(str "Hello World")))
       (h/output :stdout)))
```
