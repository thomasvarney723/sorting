(defn remove-once [value coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (if (= value (first coll))
       (rest coll)
       (cons (first coll) (remove-once value (rest coll)))))))

(defn merge-back
  ([coll1] coll1)
  ([coll1 coll2]
   (loop [out []
          [f1 & r1 :as c1] coll1
          [f2 & r2 :as c2] coll2]
     (cond (empty? c1) (concat out c2)
           (empty? c2) (concat out c1)
           :else
           (let [least (min f1 f2)]
             (cond (= least f1)
                   (recur (conj out f1)
                          (remove-once f1 c1)
                          c2)
                   (= least f2)
                   (recur (conj out f2)
                          (remove-once f2 c2)
                          c1)))))))
               

(defn merge-sort [coll]
  (let [part #(partition-all 2 %)
        map-apply-merge (partial map #(apply merge-back %))]
    (->> (reductions #(%2 %1)
                    (map sort (part coll))
                    (cycle [part map-apply-merge]))
         (drop-while #(> (count %) 1))
         first
         map-apply-merge
         first)))
