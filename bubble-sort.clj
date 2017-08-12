(defn bubble [[x & xs]]
  (reduce (fn [result input]
            (let [[lesser greater] ((juxt min max) (peek result) input)]
              (conj (pop result) lesser greater)))
          [x]
          xs))

(defn bubble-sort [coll]
  (if (empty? coll)
    coll
    (let [bubbled (bubble coll)]
      (if (= coll bubbled)
        coll
        (lazy-cat (bubble-sort (pop bubbled))
                  [(peek bubbled)])))))
