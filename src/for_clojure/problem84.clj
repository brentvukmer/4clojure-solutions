(ns for-clojure.problem84)

; Write a function which generates the transitive closure of a binary relation.
;
; The relation will be represented as a set of 2 item vectors.

(comment

  (let [divides #{[8 4] [9 3] [4 2] [27 9]}]
    (= (__ divides) #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]}))

  (let [more-legs
        #{["cat" "man"] ["man" "snake"] ["spider" "cat"]}]
    (= (__ more-legs)
       #{["cat" "man"] ["cat" "snake"] ["man" "snake"]
         ["spider" "cat"] ["spider" "man"] ["spider" "snake"]}))

  (let [progeny
        #{["father" "son"] ["uncle" "cousin"] ["son" "grandson"]}]
    (= (__ progeny)
       #{["father" "son"] ["father" "grandson"]
         ["uncle" "cousin"] ["son" "grandson"]}))

  (let [progeny
        #{["father" "son"] ["uncle" "cousin"] ["son" "grandson"]}]
    (= (__ progeny)
       #{["father" "son"] ["father" "grandson"]
         ["uncle" "cousin"] ["son" "grandson"]})))


(defn transitive-closures
  [binary-relations]
  (loop [relations binary-relations
         count 0]
    (let [indexed (into {} relations)
          transitive-pairs (map #(vector (first %) (get indexed (second %))) indexed)
          transitives (filter #(not (nil? (second %))) transitive-pairs)
          updated-relations (set (concat relations transitives))]
      (if (= relations updated-relations)
        relations
        (recur updated-relations
               (inc count))))))