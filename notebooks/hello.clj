;; # Hello, Clerk! 👋

;; grab some words from the UNIX dictionary
(def words
  ["bob"
   "sue"
   "joe"
   "sam"
   "jim"
   "ann"
   "ben"])

;; Function to render an xf-net as svg
(defn render-net [net-xf]
  (-> (arr/create-graph)
      (arr/with-graph
        (net/postwalk
          net-xf :input
          (fn [[v-type v-id] _ v2s]
            (doto (arr/insert-vertex!
                    v-id
                    :shape (if (= v-type :node) "ellipse" "rectangle")
                    :perimeter (if (= v-type :node) "ellipsePerimeter" "rectanglePerimeter")
                    :stroke-width 2
                    :stroke-color "black")
              (as-> v1
                (doseq [v2 v2s]
                  (arr/insert-edge! v1 v2
                    :rounded true
                    :stroke-color "black"
                    :stroke-width 2)))))))
      arr/as-svg
      clerk/html))

(clerk/table
  (for [ex [hw/ex2 hw/ex3 hw/ex4 hw/ex5 hw/ex6 hw/ex7 hw/ex8 hw/ex9 hw/ex10 hw/ex11
              hw/ex12 hw/ex13 hw/ex14 hw/ex15]]
     [(render-net ex) (net/pr-net ex)]))
