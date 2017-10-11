(defn merge-seqs
  ([coll] coll)
  ([[h1 & t1 :as c1] [h2 & t2 :as c2]]
   (cond (not h1) c2
         (not h2) c1
         :else (lazy-seq
                  (if (<= h1 h2) 
                    (cons h1 (merge-seqs t1 c2))
                    (cons h2 (merge-seqs t2 c1)))))))

(defn merge-back [coll]
  (map #(apply merge-seqs %) (partition-all 2 coll)))
                              
(defn merge-sort [coll]
  (->> (iterate merge-back (map vector coll))
       (drop-while #(> (count %) 1))
       ffirst))
