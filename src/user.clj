(require '[nextjournal.clerk :as clerk])
(require '[arrowic.core :as arr])
(require '[net.n01se.hassle.core :as net])
(require '[net.n01se.hassle.transducers :refer [final multiplex demultiplex tag detag] :as t])
(require '[net.n01se.hassle.hw :as hw])

(def styles
  {:node [:shape "ellipse"
          :perimeter "ellipsePerimeter"
          :fill-color "lightyellow"
          :stroke-width 2
          :stroke-color "black"]
   :input [:shape "rectangle"
           :perimeter "rectanglePerimeter"
           :fill-color "lightblue"
           :stroke-width 2
           :stroke-color "black"]
   :output [:shape "rectangle"
            :perimeter "rectanglePerimeter"
            :fill-color "orange"
            :stroke-width 2
            :stroke-color "black"]})

(defn render-net [net-xf]
  (-> (arr/create-graph)
      (arr/with-graph
        (net/postwalk
          net-xf :input
          (fn [[v-type v-id :as path] node v2s]
            (doto (apply arr/insert-vertex! v-id (styles v-type))
              (as-> v1
                (doseq [v2 v2s]
                  (arr/insert-edge!
                    v1 v2
                    :stroke-width 2
                    :stroke-color "black")))))))
      arr/as-svg
      clerk/html))

(clerk/serve! {:browse? true})
(clerk/serve! {:watch-paths ["notebooks" "src"]})
(clerk/show! "notebooks/hello.clj")
