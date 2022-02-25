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
        (arr/insert-vertex! (:label (net-xf)))
        (net/postwalk
          net-xf :input
          (fn [[v-type v-id :as path] node v2s]
            (doto (apply arr/insert-vertex!
                         (node :label v-id)
                         (styles v-type))
              (as-> v1
                (doseq [v2 v2s]
                  (arr/insert-edge!
                    v1 v2
                    :stroke-width 2
                    :stroke-color "black")))))))
      arr/as-svg
      clerk/html))

(defn render-xf-compose-tree []
 (-> (arr/create-graph)
     (arr/with-graph
       (let [v-map (arr/insert-vertex! "map")
             v-take-while (arr/insert-vertex! "take-while")
             v-filter (arr/insert-vertex! "filter")
             v-final (arr/insert-vertex! "final" :fill-color "lightblue")
             v-tag (arr/insert-vertex! "tag" :fill-color "lightblue")
             v-detag (arr/insert-vertex! "detag" :fill-color "lightblue")
             v-multiplex (arr/insert-vertex! "multiplex" :fill-color "lightblue")
             v-demultiplex (arr/insert-vertex! "demultiplex" :fill-color "lightblue")
             v-net (arr/insert-vertex! "net" :fill-color "lightblue")]
         (arr/insert-edge! v-tag v-net)
         (arr/insert-edge! v-detag v-net)
         (arr/insert-edge! v-multiplex v-net)
         (arr/insert-edge! v-demultiplex v-net)
         (arr/insert-edge! v-final v-tag)
         (arr/insert-edge! v-filter v-detag)
         (arr/insert-edge! v-take-while v-detag)
         (arr/insert-edge! v-map v-detag)))

     arr/as-svg
     clerk/html))

(defn build-static-app! []
  (clerk/build-static-app! {:paths (map #(str "docs/" % ".clj")
                                        ["meeting-intro"
                                         "xf-nets"
                                         "transducers"
                                         "meeting-outro"])
                            :bundle? false}))

(clerk/serve! {:browse? true})
(clerk/serve! {:watch-paths ["docs" "src"]})
(clerk/show! "docs/xf-nets.clj")
