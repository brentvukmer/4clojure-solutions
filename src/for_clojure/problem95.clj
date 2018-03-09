(ns for-clojure.problem95)

;
; To Tree, or not to Tree
; http://www.4clojure.com/problem/95
;
; Difficulty:	Easy
; Topics:	trees
;
; Write a predicate which checks whether or not a given sequence represents a binary tree.
; Each node in the tree must have a value, a left child, and a right child.
;


(defn binary-tree?
  [x]
  (if (coll? x)
    (and
      (= 3 (count x))
      (not (coll? (first x)))
      (binary-tree? (second x))
      (binary-tree? (nth x 2)))
    (nil? x)))


(defn run-tests
  []

  [(= (binary-tree? '(:a (:b nil nil) nil))
      true)

   (= (binary-tree? '(:a (:b nil nil) nil))
      true)

   (= (binary-tree? '(:a (:b nil nil)))
      false)

   (= (binary-tree? [1 nil [2 [3 nil nil] [4 nil nil]]])
      true)

   (= (binary-tree? [1 [2 nil nil] [3 nil nil] [4 nil nil]])
      false)

   (= (binary-tree? [1 [2 [3 [4 nil nil] nil] nil] nil])
      true)

   (= (binary-tree? [1 [2 [3 [4 false nil] nil] nil] nil])
      false)

   (= (binary-tree? '(:a nil ()))
      false)

   (= (binary-tree? '(:a nil ()))
      false)])


