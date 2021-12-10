# Hassle

An experimental reactive functional library for Clojure. The primary goal is to
demonstrate the ability to express programs as a DAG of functions over streams of events.

```clojure
#_traditional
(defn main []
  (println "Hello World!"))

#_hassle
(def main
  (h/net {init :h/init}
         greeting (h/assign init "Hello World!")
         {:h/stdout greeting}))
```
