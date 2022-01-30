(require '[nextjournal.clerk :as clerk])
(require '[arrowic.core :as arr])
(require '[net.n01se.hassle.net :refer [tag detag net] :as net])
(require '[net.n01se.hassle.transducers :refer [final multiplex demultiplex] :as t])
(require '[net.n01se.hassle.hw :as hw])

(def styles
  {:node [:shape "ellipse"
          :perimeter "ellipsePerimeter"
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
   
(defn render-edge [v1 v2]
  (arr/insert-edge!
    v1 v2
    :rounded true
    :stroke-width 2
    :stroke-color "black"))

(defn render-net [net-xf]
  (-> (arr/create-graph)
      (arr/with-graph
        (net/postwalk
          net-xf :input
          (fn [[v-type v-id :as path] node v2s]
            (doto (apply arr/insert-vertex! v-id (styles v-type))
              (as-> v1
                (doseq [v2 v2s]
                  (render-edge v1 v2)))))))
                  
      arr/as-svg
      clerk/html))

(clerk/serve! {:browse? true})
(clerk/serve! {:watch-paths ["notebooks" "src"]})
(clerk/show! "notebooks/hello.clj")
