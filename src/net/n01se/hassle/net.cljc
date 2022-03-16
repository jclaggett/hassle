(ns net.n01se.hassle.net)

(defn normalize-inputs [inputs]
  (if (set? inputs)
    (mapcat normalize-inputs inputs)
    (if (nil? inputs)
      (list)
      (list inputs))))

(defn normalize-net
  [label trees]
  (letfn [(walk-trees [net-map trees super-path]
            (reduce
              (fn [net-map [tree-type id sub-trees value label]]
                (let [node-path [tree-type id]]
                  (cond-> net-map
                    (not (nil? value))
                    (assoc-in (conj node-path :value) value)

                    (not (nil? label))
                    (assoc-in (conj node-path :label) label)

                    (not (or (= tree-type :output) (nil? super-path)))
                    (-> (update-in (conj node-path :outputs) (fnil conj #{}) super-path)
                        (update-in (conj super-path :inputs) (fnil conj #{}) node-path))

                    true
                    (walk-trees sub-trees node-path))))
              net-map
              (normalize-inputs trees)))]
    (walk-trees {:label label} trees nil)))

(defn postwalk [orig-net-map roots update-fn]
  (let [kids (case roots :input :outputs :inputs)
        root-paths (for [k (-> orig-net-map roots keys)]
                     [roots k])]

    (letfn [(update-node [net-map path]
              (update-in
                net-map path
                (fn [node]
                  (update-fn
                    path node
                    (for [kid-path (kids node)]
                      (get-in net-map kid-path))))))

            (visit-node [net-map path]
              (if (-> net-map meta ::visited (contains? path))
                net-map
                (-> net-map
                    (vary-meta update ::visited conj path)
                    (visit-nodes (get-in net-map (conj path kids)))
                    (update-node path))))

            (visit-nodes [net-map paths]
              (reduce visit-node net-map paths))]

      (-> orig-net-map
        (vary-meta assoc ::visited #{})
        (visit-nodes root-paths))))) 

(defn assert-no-outputs [inputs]
  (assert (->> inputs normalize-inputs (map first) (every? #{:input :node}))
          "Output nodes are not allowed as inputs")
  inputs)

(defn input [k] (list :input k #{}))
(defn output [k inputs] (list :output k (assert-no-outputs inputs)))

(defn node* [label value inputs]
  (list :node
        (gensym 'n)
        (assert-no-outputs inputs)
        value
        label))

(defn embed [net-map input-map]
  (-> net-map
      (postwalk
        :output
        (fn [[node-type node-id] {value :value label :label} inputs]
          (condp = node-type
            :input (input-map node-id)
            :node (node* label value (set inputs))
            :output (set inputs))))
      :output))

;; printing/debugging
(defn compact-net-map [net-map]
  (letfn [(compact-paths [paths] (map second paths))]
    (concat
      (map (fn [[k {:keys [outputs]}]]
             [k :outputs (compact-paths outputs)])
           (:input net-map))
      (map (fn [[k {:keys [outputs inputs]}]]
             [k :outputs (compact-paths outputs) :inputs (compact-paths inputs)])
           (:node net-map))
      (map (fn [[k {:keys [inputs]}]]
             [k :inputs (compact-paths inputs)])
           (:output net-map)))))
