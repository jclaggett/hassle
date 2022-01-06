(ns net.n01se.hassle.net
  (:require [clojure.pprint :refer [pprint]]))

(defn debug
  ([x] (debug x x))
  ([msg x] (println "DEBUG:" msg) x))

;; implementation
(def conj-set (fnil conj #{}))

(defn get-input-trees [x]
  (if (set? x)
    (mapcat get-input-trees x)
    (list x)))

(defn make-net-map
  [trees]
  (letfn [(make-nodes [net-map trees super-path]
            (reduce
              (fn [net-map [tree-type args sub-trees id label :as tree]]
                (condp = tree-type
                  :input (if (nil? super-path)
                           (assoc-in net-map [:inputs args :outputs] #{})
                           (-> net-map
                               (update-in [:inputs args :outputs]
                                          conj-set super-path)
                               (update-in (conj super-path :inputs)
                                          conj-set [:inputs args])))
                  :node (if (nil? super-path)
                          (-> net-map
                              (assoc-in [:nodes id :xf] args)
                              (make-nodes sub-trees [:nodes id]))
                          (-> net-map
                              (assoc-in [:nodes id :xf] args)
                              (update-in [:nodes id :outputs] conj-set super-path)
                              (update-in (conj super-path :inputs) conj-set [:nodes id])
                              (make-nodes sub-trees [:nodes id])))
                  :output (-> net-map
                              (make-nodes sub-trees [:outputs args]))))
              net-map
              (get-input-trees trees)))]
    (make-nodes {} trees nil)))

(defn postwalk-net-map [orig-net-map root update-fn]
  (let [kids (case root :inputs :outputs :outputs :inputs :none)]
    (letfn [(update-node [net-map path]
              (update-in net-map path
                         (fn [node]
                           (update-fn node path
                                      (map #(get-in net-map %)
                                           (kids node))))))

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
        (visit-nodes (->> orig-net-map root keys (map (fn [k] [root k]))))))))

(defn compact-net-map [net-map]
  (letfn [(compact-paths [paths] (map second paths))]
    (postwalk-net-map
      net-map
      :inputs
      (fn [node [node-type] _]
        (condp = node-type
          :inputs (-> node :outputs compact-paths)
          :nodes (-> node
                     (update :inputs compact-paths)
                     (update :outputs compact-paths))
          :outputs (-> node :inputs compact-paths))))))

(declare node)
(defn make-embed-fn [net-map]
  (vary-meta
    (fn embedder [input-map]
      (:outputs
        (postwalk-net-map
          net-map
          :outputs
          (fn [{xf :xf} [node-type node-id] inputs]
            (condp = node-type
              :inputs (input-map node-id)
              :nodes (node xf (set inputs))
              :outputs (set inputs))))))
    assoc ::net-map net-map))

(defn net [tree]
  (make-embed-fn (make-net-map tree)))

;; Latest attempt at a decent API
(defn input [v] (list :input v #{} (gensym 'i)))
(def inputs (reify clojure.lang.IPersistentSet
              (get [_ v] (input v))))

(defn output [k v] (list :output k v (gensym 'o)))
(defn outputs [m] (set (map (fn [[k v]] (output k v)) m)))

(defn node
  ([xf] (net (output :out (node xf (input :in)))))
  ([xf in] (list :node xf in (gensym 'n))))

(defn pr-net [f]
  (doto (-> f meta ::net-map compact-net-map)
    pprint))
