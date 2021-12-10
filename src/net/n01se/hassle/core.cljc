(ns net.n01se.hassle.core
  (:require [clojure.core.async :as cca]
            [clojure.pprint :refer [pprint]]
            [net.cgrand.xforms :as x]))

(defn input [& args] (concat [:input #{}] args))
(defn node [& args] (cons :node args))
(defn output [& args] (cons :output args))
(defn outputs [& args] (set args))

(defn merge-ch [out-chs]
  (condp = (count out-chs)
    0 nil
    1 (first out-chs)
    (cca/merge
      (for [out-ch out-chs]
        (if (satisfies? cca/Mult out-ch)
          (cca/tap out-ch (cca/chan))
          out-ch)))))

(defn connect-ch [in-ch ch]
  (if (nil? in-ch)
    ch
    (if (satisfies? cca/Mult in-ch)
      (cca/tap in-ch ch)
      (cca/pipe in-ch ch))))

(defn mult-ch [ch nodes]
  (condp = (count nodes)
    0 nil
    1 ch
    (cca/mult ch)))

(def argv [])
(def env (into {} (System/getenv)))
(def init-map {:argv argv
               :env env})
(def init-variant [:init init-map])

(defmulti io-chan identity)
(defmethod io-chan :init [_]
  (cca/to-chan! [init-map]))
(defmethod io-chan :stdout [_]
  (cca/chan 1 (map #(doto % println))))

(defn make-net-map
  ([trees] (make-net-map {:inputs {}
                          :outputs {}}
                         trees
                         nil))

  ([net-map trees super-node-key]
   (reduce
     (fn [net-map [tree-type sub-trees args :as tree]]
       (let [node-key (hash tree)]
         (cond-> net-map
           (not (contains? net-map node-key))
           (assoc node-key {:type tree-type
                            :args args
                            :inputs #{}
                            :outputs #{}})

           (= tree-type :input)
           (-> (assoc-in [:inputs args] node-key)
               (update node-key dissoc :inputs))

           (= tree-type :output)
           (-> (assoc-in [:outputs args] node-key)
               (update node-key dissoc :outputs))

           (not (nil? super-node-key))
           (-> (update-in [node-key :outputs] conj super-node-key)
               (update-in [super-node-key :inputs] conj node-key))

           true
           (make-net-map sub-trees node-key))))

     net-map
     (if (set? trees) trees #{trees}))))

(defn postwalk-net-map [orig-net-map kids-fn update-fn]
  (letfn [(update-node [net-map node-key]
            (update net-map node-key update-fn net-map))

          (visit-node [net-map node-key]
            (if (contains? (-> net-map meta ::visited) node-key)
              net-map
              (-> net-map
                  (vary-meta update ::visited conj node-key)
                  (visit-nodes (kids-fn (net-map node-key)))
                  (update-node node-key))))

          (visit-nodes [net-map node-keys]
            (reduce visit-node net-map node-keys))

          (get-root-node-keys []
            (let [roots-fn (case kids-fn
                             :inputs :outputs
                             :outputs :inputs
                             :none)]
              (-> orig-net-map roots-fn vals)))]

    (-> orig-net-map
        (vary-meta assoc ::visited #{})
        (visit-nodes (get-root-node-keys)))))

(defn get-io-chan [net-map opposites args]
  ;; Tempting to use get-in's default value but that would cause io-chan to
  ;; always be called which is bad since it creates a channel.
  (or (get-in net-map [(get (net-map opposites) args) :ch])
      (io-chan args)))

(defn asyncify-net-map [net-map]
  (postwalk-net-map
    net-map
    :inputs
    (fn [{:keys [args inputs outputs] :as node} net-map]
      (let [in-ch (merge-ch (map (comp :out-ch net-map) inputs))
            ch (connect-ch
                 in-ch
                 (condp = (:type node)
                   :input (get-io-chan net-map :outputs args)
                   :node (cca/chan 1 args)
                   :output (get-io-chan net-map :inputs args)))
            out-ch (mult-ch ch outputs)]
        (assoc node
               :in-ch in-ch
               :ch ch
               :out-ch out-ch)))))

(defn debug [msg x] (println "DEBUG:" msg) x)

(defn demultiplex #_constructor
  ([inputs xf] (if (= 1 (count inputs)) xf (demultiplex xf)))
  ([xf]
   (let [*ref-count (volatile! 0)
         *cache (volatile! {})]
     (fn transducer [rf]
       (if (< 1 (vswap! *ref-count inc))
         (:rf @*cache)
         (let [rf' (xf rf)
               rf'' (fn
                      (#_init []
                       (if (contains? @*cache :init)
                         (:init @*cache)
                         (:init (vswap! *cache assoc :init (rf')))))
                      (#_step [a v]
                       (if (contains? @*cache :reduced)
                         (@*cache :reduced)
                         (let [a' (rf' a v)]
                           (if (reduced? a')
                             (:reduced (vswap! *cache assoc :reduced a'))
                             a'))))
                      (#_fini [a]
                       (if (contains? @*cache :fini)
                         (:fini @*cache)
                         (if (or (> 1 (vswap! *ref-count dec))
                                 (contains? @*cache :reduced))
                           (:fini (vswap! *cache assoc :fini (rf' a)))
                           a))))]
           (:rf (vswap! *cache assoc :rf rf''))))))))

(defn multiplex
  [xfs]
  (if (= 1 (count xfs))
    (first xfs)
    (x/multiplex xfs)))

(defn map-vals [f coll]
  (->> coll
       (map (fn [[k v]] [k (f v)]))
       (into {})))

(defn switch-alt [xf-map]
  (multiplex
    (map (fn [[k xf]]
           (comp (filter #(= k (first %)))
                 (take-while #(= (count %) 2))
                 (map second)
                 xf))
         xf-map)))

(defn switch [xf-map]
    (fn transducer [rf]
      (let [rf-map (map-vals (fn [xf]
                               ((comp (take-while #(= (count %) 2))
                                      (map second)
                                      xf)
                                rf))
                           xf-map)
            *rf-map (volatile! rf-map)]
        (fn reducer
          (#_init [] (rf))
          (#_step [a [k :as v]]
           (let [rf (@*rf-map k (fn noop [a v] a))
                 a' (rf a v)]
             (if (reduced? a')
               (if (not (empty? (vswap! *rf-map dissoc k)))
                 @a'
                 a')
               a')))
          (#_fini [a]
           (reduce (fn [a [k rf]] (rf a))
                   a
                   @*rf-map))))))

(defn get-root-nodes [net-map root]
  (->> (net-map root)
       (map (fn [[k v]] [k (net-map v)]))
       (into {})))

(defn output-xf [k]
  (fn transducer [rf]
    (fn reducer
      (#_init [] (rf))
      (#_step [a v] (rf a [k v]))
      (#_fini [a] (rf (unreduced (rf a [k])))))))
  
(defn transduce-net-map [net-map]
  (-> net-map
      (postwalk-net-map
        :outputs
        (fn [{:keys [args inputs outputs] :as node} net-map]
          (let [output-xfs (map net-map outputs)]
            (condp = (:type node)
              :input (multiplex output-xfs)
              :node (comp (demultiplex inputs args)
                          (multiplex output-xfs))
              :output (demultiplex inputs (output-xf args))))))
      (get-root-nodes :inputs)
      switch))

(defn run-xf [net-map inputs]
  (-> net-map
      transduce-net-map
      (sequence inputs)))

(defn drain-net-map [net-map]
  (let [pure-outputs (->> (:outputs net-map)
                          (remove (fn [[k v]] (contains? (:inputs net-map) k)))
                          #_(#(do (println "Draining:" (map first %)) %))
                          (map (comp :ch net-map last)))]
    (-> (merge-ch pure-outputs)
        (connect-ch (cca/chan (cca/dropping-buffer 0))))
    net-map))

(defn start [net-map]
  (-> net-map
      asyncify-net-map
      drain-net-map))

(defn collect-io-chans [net-map]
  (->> (merge (net-map :inputs)
              (net-map :outputs))
       (map (fn [[k v]] [k (get-in net-map [v :ch])]))
       (into {})))

(defmacro net [& args]
  (let [inputs (first args)
        typed-inputs (mapcat (fn [[k v]] [k (list `list :input #{} v)]) inputs)
        body (drop-last (rest args))
        outputs (last args)
        typed-outputs (map (fn [[k v]] (list `list :output v k)) outputs)]
    `^{::compose-net (fn [~inputs] (let [~@body] ~outputs))}
    (let [~@typed-inputs ~@body] (make-net-map #{~@typed-outputs}))))
  
