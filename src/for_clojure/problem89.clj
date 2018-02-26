(ns for-clojure.problem89
  (:require [clojure.set :as set]))

;
; Graph Tour
; http://www.4clojure.com/problem/89
;
; Difficulty:	Hard
; Topics:	graph-theory
;
; Starting with a graph you must write a function that returns true
; if it is possible to make a tour of the graph in which every edge
; is visited exactly once.
;
; The graph is represented by a vector of tuples, where each tuple
; represents a single edge.
;
; The rules are:
;
; - You can start at any node.
; - You must visit each edge exactly once.
; - All edges are undirected.
;


(defn find-match
  [remaining path]
  (let [current (last path)
        possibles (filter #(= 1 (count (set/intersection (set current) (set %)))) remaining)]
    (if (= 1 (count path))
      (first possibles)
      (first (filter #(not= (set/intersection (set current) (set %))
                            (set/intersection (set (first (take-last 2 path))) (set %))) possibles)))))


(defn tour-path-for
  [edge other-edges]
  (loop [current-edge edge
         remaining other-edges
         path [current-edge]]
    (if (empty? remaining)
      path
      (let [match (find-match remaining path)]
        (if (nil? match)
          path
          (recur match
                 (remove #{current-edge match} remaining)
                 (conj path match)))))))

(defn tour-paths
  [edges]
  (filter #(= (count %) (count edges))
          (map #(tour-path-for % (remove #{%} edges)) edges)))


(defn tourable?
  [edges]
  (not (empty? (tour-paths edges))))


(comment

  (= true (__ [[:a :b]]))

  (= false (__ [[:a :a] [:b :b]]))

  (= false (__ [[:a :b] [:a :b] [:a :c] [:c :a]
                [:a :d] [:b :d] [:c :d]]))

  (= true (__ [[1 2] [2 3] [3 4] [4 1]]))

  (= true (__ [[:a :b] [:a :c] [:c :b] [:a :e]
               [:b :e] [:a :d] [:b :d] [:c :e]
               [:d :e] [:c :f] [:d :f]]))

  (= false (__ [[1 2] [2 3] [2 4] [2 5]]))

  (fn [edges] (let [find-match (fn
                                 [remaining path]
                                 (let [current (last path)
                                       possibles (filter #(= 1 (count (clojure.set/intersection (set current) (set %)))) remaining)]
                                   (if (= 1 (count path))
                                     (first possibles)
                                     (first (filter #(not= (clojure.set/intersection (set current) (set %))
                                                           (clojure.set/intersection (set (first (take-last 2 path))) (set %))) possibles)))))

                    tour-path-for (fn
                                    [edge other-edges]
                                    (loop [current-edge edge
                                           remaining other-edges
                                           path [current-edge]]
                                      (if (empty? remaining)
                                        path
                                        (let [match (find-match remaining path)]
                                          (if (nil? match)
                                            path
                                            (recur match
                                                   (remove #{current-edge match} remaining)
                                                   (conj path match)))))))
                    tour-paths (fn
                                 [edges]
                                 (filter #(= (count %) (count edges))
                                         (map #(tour-path-for % (remove #{%} edges)) edges)))]
                (not (empty? (tour-paths edges)))))

  )
