(ns net.n01se.hassle.core
  (:require [clojure.core.async :as cca]
            [clojure.pprint :refer [pprint]]))

(defn debug [msg x] (println "DEBUG:" msg) x)

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
     (fn [net-map [tree-type sub-trees args label :as tree]]
       (let [node-key (hash tree)]
         (cond-> net-map
           (not (contains? net-map node-key))
           (assoc node-key {:type tree-type
                            :args args
                            :label label
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

;; Transducer specific code
(defmacro reducer
  [[init & init-body]
   [step & step-body]
   [fini & fini-body]]
  (assert (= init 'init) (str "Expected init but got: " init))
  (assert (= step 'step) (str "Expected step but got: " step))
  (assert (= fini 'fini) (str "Expected fini but got: " fini))
  `(fn ~'reducer
     ~init-body
     ~step-body
     ~fini-body))
(defmacro init [rf] `(~rf))
(defmacro step [rf a v] `(~rf ~a ~v))
(defmacro fini [rf a] `(~rf ~a))

(defn multiplex [xfs]
  (fn transducer [rf]
    (let [rf-map (->> xfs
                      (map-indexed (fn [i xf] [i (xf rf)]))
                      (into (sorted-map)))
          *rf-map (volatile! rf-map)]
      (reducer
        (init [] (init rf))
        (step [a v]
              (reduce
                (fn [a [k rf]]
                  (let [a' (step rf a v)]
                    (if (reduced? a')
                      (let [a'' (fini rf (unreduced a'))]
                        (if (empty? (vswap! *rf-map dissoc k))
                          (reduced a'')
                          (unreduced a'')))
                      a')))
                a
                @*rf-map))
        (fini [a]
              (reduce (fn [a [_ rf]] (fini rf a))
                      a
                      @*rf-map))))))

(defn demultiplex [xf]
  (let [*ref-count (volatile! 0)
        *cache (volatile! {})
        label (-> xf meta :label)]
    (fn transducer [rf]
      (vswap! *ref-count inc)
      (if (contains? @*cache :rf)
        (:rf @*cache)
        (let [rf' (xf rf)
              rf'' (reducer
                     (init []
                           (if (contains? @*cache :init)
                             (:init @*cache)
                             (:init (vswap! *cache assoc :init (init rf')))))
                     (step [a v]
                           (if (contains? @*cache :reduced)
                             (:reduced @*cache)
                             (let [a' (step rf' a v)]
                               (if (reduced? a')
                                 (:reduced (vswap! *cache assoc :reduced a'))
                                 a'))))
                     (fini [a]
                           (if (contains? @*cache :fini)
                             (:fini @*cache)
                             (if (or (zero? (vswap! *ref-count dec))
                                     (contains? @*cache :reduced))
                               (:fini (vswap! *cache assoc :fini (fini rf' a)))
                               a))))]
          (:rf (vswap! *cache assoc :rf rf'')))))))

(defn map-vals [f coll]
  (->> coll
       (map (fn [[k v]] [k (f v)]))
       (into {})))


(defn final
  ([x]
   (fn transducer [rf]
     (reducer
       (init [] (init []))
       (step [a v] (step rf a v))
       (fini [a] (fini rf (unreduced (step rf a x)))))))
  ([x xs] (sequence (final x) xs)))

(defn vstream
  ([k] (comp (map (fn [x] [k x]))
             (final [k])))
  ([k xs] (sequence (vstream k) xs)))

(defn switch-vstreams-alt
  ([xf-map]
   (->> xf-map
        (map (fn [[k xf]]
               (comp (filter #(= (first %) k))
                     (take-while #(= (count %) 2))
                     (map second)
                     xf)))
        multiplex))
  ([xf-map xs] (sequence (switch-vstreams-alt xf-map) xs)))

(defn switch-vstreams
  ([xf-map]
   (fn transducer [rf]
     (let [rf-map (map-vals (fn [xf]
                              ((comp (take-while #(= (count %) 2))
                                     (map second)
                                     xf)
                               rf))
                            xf-map)
           *rf-map (volatile! rf-map)]
       (reducer
         (init [] (init rf))
         (step [a [k :as v]]
          (let [rf (get @*rf-map k (fn no-op-step [a v] a))
                a' (step rf a v)]
            (if (reduced? a')
              ;; dissoc and finish this entry
              (let [a'' (fini rf (unreduced a'))]
                (if (empty? (vswap! *rf-map dissoc k))
                  (reduced a'')
                  (unreduced a'')))
              a')))
         (fini [a]
          (reduce (fn [a [_ rf]] (fini rf a))
                  a
                  @*rf-map))))))
  ([xf-map xs] (sequence (switch-vstreams xf-map) xs)))

(defn get-root-nodes [net-map root]
  (->> (net-map root)
       (map (fn [[k v]] [k (net-map v)]))
       (into {})))

(defn dagduce [net-map]
  (-> net-map
      (postwalk-net-map
        :outputs
        (fn [{:keys [args inputs outputs label] :as node} net-map]
          (let [label (if (nil? label) args label)
                output-xfs (vary-meta (map net-map outputs) assoc :label label)
                multiplex' (if (= (count outputs) 1) first multiplex)
                demultiplex' (if (= (count inputs) 1) identity demultiplex)]
            (vary-meta
              (condp = (:type node)
                :input (multiplex' output-xfs)
                :node (demultiplex' (vary-meta
                                      (comp args (multiplex' output-xfs))
                                      assoc :label label))
                :output (demultiplex' (vary-meta
                                        (vstream args)
                                        assoc :label label)))
              assoc :label label))))
      (get-root-nodes :inputs)
      switch-vstreams))

(defn run-xf [net-map inputs]
  (-> net-map
      dagduce
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
