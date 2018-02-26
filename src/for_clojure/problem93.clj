(ns fourclojure-solutions.clojure93)

;
; Partially Flatten a Sequence
; http://www.4clojure.com/problem/93
;
; Difficulty:	Medium
; Topics:	seqs
;
;
;Write a function which flattens any nested combination of sequential things
; (lists, vectors, etc.), but maintains the lowest level sequential items.
;
; The result should be a sequence of sequences with only one level of nesting.
;


(defn unwrap
  [nested]
  (filter #(= % (filter (fn [x] (not (coll? x))) %))
          (filter coll?
                  (tree-seq coll? seq nested))))


(defn run-tests
  []

  [(= (unwrap [["Do"] ["Nothing"]])
      [["Do"] ["Nothing"]])

   (= (unwrap [[[[:a :b]]] [[:c :d]] [:e :f]])
      [[:a :b] [:c :d] [:e :f]])

   (= (unwrap '((1 2) ((3 4) ((((5 6)))))))
      '((1 2) (3 4) (5 6)))])


(comment
  (fn [nested]
    (filter #(= % (filter (fn [x] (not (coll? x))) %))
            (filter coll?
                    (tree-seq coll? seq nested)))))