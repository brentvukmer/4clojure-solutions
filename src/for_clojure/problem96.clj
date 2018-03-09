(ns for-clojure.problem96
  (:require [for-clojure.problem95 :as p95]))
;
; Beauty is Symmetry
;
; Difficulty:	Easy
; Topics:	trees
;
;
; Let us define a binary tree as "symmetric" if
; the left half of the tree is the mirror image
; of the right half of the tree.
;
; Write a predicate to determine whether or not
; a given binary tree is symmetric.
;
; (See problem 95 for a reminder on the tree representation we're using).
;

(defn reverse-binary-tree
  [x]
  (if (not (coll? x))
    x
    (vector (first x)
            (reverse-binary-tree (nth x 2))
            (reverse-binary-tree (second x)))))


(defn symmetric-binary-tree?
  [x]
  (and (p95/binary-tree? x)
       (= (reverse-binary-tree (second x))
          (nth x 2))))


(defn run-tests
  []
  [(= (symmetric-binary-tree? '(:a (:b nil nil) (:b nil nil))) true)

   (= (symmetric-binary-tree? '(:a (:b nil nil) nil)) false)

   (= (symmetric-binary-tree? '(:a (:b nil nil) (:c nil nil))) false)

   (= (symmetric-binary-tree? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                               [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
      true)

   (= (symmetric-binary-tree? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                               [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
      false)

   (= (symmetric-binary-tree? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                               [2 [3 nil [4 [6 nil nil] nil]] nil]])
      false)])




