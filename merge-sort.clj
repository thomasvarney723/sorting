(defn merge-back 
  ([coll] coll)
  ([coll1 coll2]
   (cond (empty? coll1) coll2
         (empty? coll2) coll1
         :else (lazy-seq
                (let [[f1 & r1 :as c1] (seq coll1)
                      [f2 & r2 :as c2] (seq coll2)]
                  (if (<= f1 f2) 
                    (cons f1 (merge-back r1 c2))
                    (cons f2 (merge-back r2 c1))))))))
                              
(defn merge-sort [coll]
  (letfn [(part [s]
             (partition-all 2 s))
          (map-apply-merge [s]
             (map #(apply merge-back %) s))]
    (->> (iterate (comp map-apply-merge part) (map vector coll))
         (drop-while #(> (count %) 1))
         ffirst))
