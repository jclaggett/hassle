(ns net.n01se.hassle.core
  (:require [clojure.core.async :as cca]
            [clojure.pprint :refer [pprint]]

            [net.n01se.hassle.net :refer [postwalk-net-map]]))

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
