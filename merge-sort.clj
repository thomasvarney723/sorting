(defn rpart [[x & xs]]
    (reduce (fn [result input]
              (if (<= (peek (peek result)) input)
                (conj (pop result) (conj (peek result) input))
                (conj result [input])))
            [[x]]
            xs))

(defn sorted-runs [coll]
  (let [previous (atom Integer/MIN_VALUE)]
    (partition-by
     (fn [n]
       (if (<= @previous n)
         (do (reset! previous n) :no-split)
         (do (reset! previous n) :split)))
     coll)))

(defn merge-back
  ([coll] coll)
  ([[f1 & r1 :as c1] [f2 & r2 :as c2]]
   (lazy-seq
    (cond (not f1) c2
          (not f2) c1
          (<  f1 f2)  (cons f1 (merge-back c2 r1))
          (>= f1 f2)  (cons f2 (merge-back c1 r2)))))
  ([c1 c2 & more]
   (reduce merge-back [] more)))

(defn merge-sort [coll]
    (apply merge-back (sorted-runs coll)))
