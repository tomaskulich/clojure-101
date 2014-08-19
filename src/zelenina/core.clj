(ns zelenina.core
  (:gen-class))

(require 'clojure.set)

(defn get_random_neighs [n p]
  (into [] (for [i (range n) :when (< (rand) p )] i)))

(defn get_random_graph [n p] 
  (into {} (for [i (range n)] [i (get_random_neighs n p)] )))

(defn shortest_path_to_good [init neighs good?]
  (defn new_neighs [point visited]
    (clojure.set/difference (set (neighs point)) (set visited)))

  (defn path [from to visited]
    (defn _path [cp cf]
      (if (= cf from) cp
        (let [ccf (visited (first cp))]
          (_path (cons ccf cp) ccf))))
    (_path [to] to)
  )

  (defn sptg [v tv]
    (do
      (let [c (first tv)]
        (if (good? c) (path (init) c v)
          (do
            (println c)
            (sptg (into {} (for [k (concat (neighs c) (keys v))] [k (if (v k) (v k) c)])) (rest (concat tv (new_neighs c (keys v))))))))))

  (sptg {(init) nil} [(init)]))


(defn marcelkina_fn [init neighs good?]
  (defn new_neighs [point visited]
    (clojure.set/difference (set (neighs point)) (set visited)))

  (defn path [from to visited]
    (defn _path [cp cf]
      (if (= cf from) cp
        (let [ccf (visited (first cp))]
          (_path (cons ccf cp) ccf))))
    (_path [to] to)
  )

  (defn sptg [v tv]
    (do
      (let [c (first tv)]
        (if (good? c) {:final true :result (path (init) c v)}
          (do
            (println c)
            {:final false :run (fn []
              sptg
              (into {} (for [k (concat (neighs c) (keys v))] [k (if (v k) (v k) c)]))
              (rest (concat tv (new_neighs c (keys v)))))})))))

  (sptg {(init) nil} [(init)]))

;{final run result}

;(defn get_furthest [graph start]
;  (defn unvisited_neighs [point visited]
;    (clojure.set/difference (set (graph point)) (union visited (set q)))
;
;  (defn furthest [v q l]
;    (do
;      (if (empty? q) l
;        (let [n (first q)]
;          (furthest (conj v n) (rest (concat q (unvisited_neighs n v))) n)))))
;  (furthest (sorted-set start) [start] nil)))

(defn run_until_done [run]
  (let [result (run)]
    (if (:final result) (:result result) ((:run result)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [g (get_random_graph 100 0.05)]
    (println g)
    (println (run_until_done (marcelkina_fn (fn [] 0)
                            g
                            (fn [x] (and (not (= nil x)) (= (mod x 7) 1)))
                            )))))
