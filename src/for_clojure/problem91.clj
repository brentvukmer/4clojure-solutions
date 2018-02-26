(ns for-clojure.problem91
  (:require [for-clojure.problem89 :as graphs]))

;
; Graph Connectivity
; http://www.4clojure.com/problem/91
;
; Difficulty:	Hard
; Topics:	graph-theory
;
;
; Given a graph, determine whether the graph is connected.
; A connected graph is such that a path exists between any two given nodes.
;
; Your function must return true if the graph is connected and false otherwise.
;
; You will be given a set of tuples representing the edges of a graph, each
; member of a tuple being a vertex/node in the graph.
;
; Each edge is undirected (can be traversed either direction).
;


(defn indexed
  [graph]
  (reduce (fn [accum item] (let [node1 (first item)
                                 entry1 (get accum node1 [])
                                 node2 (second item)
                                 entry2 (get accum node2 [])]
                             (assoc accum node1 (conj entry1 node2) node2 (conj entry2 node1)))) {} graph))

(defn paths
  [node indexed num-nodes]
  (let [seen (atom #{})]
    (take-while #(< (count (conj @seen %)) (inc num-nodes))
                (tree-seq #(when-not (@seen %) (swap! seen conj %))
                          #(get indexed %)
                          node))))


(defn connected?
  [graph]
  (let [tree-info (indexed graph)
        nodes (set (flatten (vec tree-info)))
        node-paths (map #(set (paths % tree-info (count nodes))) nodes)]
    (empty? (filter #(not= nodes (set %)) node-paths))))


(comment

  (= true (__ #{[:a :a]}))

  (= true (__ #{[:a :b]}))

  (= false (__ #{[1 2] [2 3] [3 1]
                 [4 5] [5 6] [6 4]}))

  (= true (__ #{[1 2] [2 3] [3 1]
                [4 5] [5 6] [6 4] [3 4]}))

  (= false (__ #{[:a :b] [:b :c] [:c :d]
                 [:x :y] [:d :a] [:b :e]}))

  (= true (__ #{[:a :b] [:b :c] [:c :d]
                [:x :y] [:d :a] [:b :e] [:x :a]}))

  (= true (__ #{[:a :b] [:b :c] [:c :d]
                [:x :y] [:d :a] [:b :e] [:x :a]}))

  (fn [graph]
    (let [index-fn (fn
                    [graph]
                    (reduce (fn [accum item] (let [node1 (first item)
                                                   entry1 (get accum node1 [])
                                                   node2 (second item)
                                                   entry2 (get accum node2 [])]
                                               (assoc accum node1 (conj entry1 node2) node2 (conj entry2 node1)))) {} graph))
          paths-fn (fn
                  [node indexed num-nodes]
                  (let [seen (atom #{})]
                    (take-while #(< (count (conj @seen %)) (inc num-nodes))
                                (tree-seq #(when-not (@seen %) (swap! seen conj %))
                                          #(get indexed %)
                                          node))))
          connected? (fn
                       [graph]
                       (let [tree-info (index-fn graph)
                             nodes (set (flatten (vec tree-info)))
                             node-paths (map #(set (paths-fn % tree-info (count nodes))) nodes)]
                         (empty? (filter #(not= nodes (set %)) node-paths))))]
      (connected? graph)))

  )