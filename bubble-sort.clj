(defn pass [coll]
  (reduce (fn [stack input]
            (cond
              (empty? stack)
              [input]
              (< (peek stack) input)
              (conj stack input)
              :else
              (conj (pop stack) input (peek stack))))
          []
          coll))

(defn bubble-sort [coll]
  (let [passed (pass coll)]
    (if (= coll (pass coll))
      coll
      (recur passed))))
