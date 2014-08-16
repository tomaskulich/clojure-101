(ns zelenina.core
  (:gen-class))

(defn get_random_neighs [n p]
  (into [] (for [i (range n) :when (< (rand) p )] i)))

(defn get_random_graph [n p] 
  (into {} (for [i (range n)] [i (get_random_neighs n p)] )))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (get_random_graph 100 0.05)))
