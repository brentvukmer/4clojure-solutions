(ns for-clojure.problem85)

;
; Write a function which generates the power set of a given set.
; The power set of a set x is the set of all subsets of x, including the empty set and x itself.
;

(comment

  (= (__ #{1 :a}) #{#{1 :a} #{:a} #{} #{1}})

  (= (__ #{}) #{#{}})

  (= (__ #{1 2 3})
     #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})

  (= (count (__ (into #{} (range 10)))) 1024))


(defn power-set
  [s]
  (loop [subsets (into #{} (map #(hash-set %) s))]
    (if (or (empty? subsets)
            (contains? subsets s))
      (conj subsets #{})
      (recur (set (mapcat #(map (fn [x] (into % x)) subsets) subsets))))))