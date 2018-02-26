(ns for-clojure.problem82
  (:require [clojure.set :as set]
            [clojure.string :as str]))

;
; http://www.4clojure.com/problem/82
;
; A word chain consists of a set of words ordered so that each word differs by only one letter from the words directly before and after it.
;
; The one letter difference can be either an insertion, a deletion, or a substitution.
;
; Here is an example word chain:
;
;     cat -> cot -> coat -> oat -> hat -> hot -> hog -> dog
;
; Write a function which takes a sequence of words, and returns true if they can be arranged into one continous word chain, and false if they cannot.
;


(comment
  (= true (__ #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}))
  (= false (__ #{"cot" "hot" "bat" "fat"}))
  (= false (__ #{"to" "top" "stop" "tops" "toss"}))
  (= true (__ #{"spout" "do" "pot" "pout" "spot" "dot"}))
  (= true (__ #{"share" "hares" "shares" "hare" "are"}))
  (= false (__ #{"share" "hares" "hare" "are"}))
  (= false (__ #{"share" "hares" "hare" "are"})))

(def input1 #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"})

(def input2 #{"cot" "hot" "bat" "fat"})

(def input3 #{"to" "top" "stop" "tops" "toss"})

(def input4 #{"spout" "do" "pot" "pout" "spot" "dot"})

(def input5 #{"share" "hares" "shares" "hare" "are"})

(def input6 #{"share" "hares" "hare" "are"})

(def input7 #{"share" "hares" "hare" "are"})

;
; For each string in the list, find the matches in the rest of the list (i.e., string pairs whose set difference is 1 character and whose length is 1 more than the given string.)
;

(defn off-by-one-or-equal-length?
  [x y]
  (<= (Math/abs (- (count x) (count y))) 1))


(defn substituted?
  [x y]
  (= 1 (get (frequencies (map = x y)) false)))


(defn includes-all-chars?
  [s1 s2]
  (= s2 (apply str (filter #(str/includes? s1 (str %)) s2))))


(defn connectable?
  [x y]
  (cond
    (not (off-by-one-or-equal-length? x y))
    false
    (= (count x) (count y))
    (substituted? x y)
    (< (count x) (count y))
    (includes-all-chars? y x)
    (> (count x) (count y))
    (includes-all-chars? x y)))


(defn find-matches
  [item other-items]
  (let [matches (filter some?
                        (map #(when (connectable? item %) %)
                             (remove #(= item %)
                                     other-items)))]
    [item (vec matches)]))


(defn indexed
  [inputs]
  (into {} (mapv #(find-matches % inputs) inputs)))


(defn tree-paths
  [inputs]
  (let [indexes (indexed inputs)]
    (tree-seq #(some? (seq %)) #(map (fn [t] (conj % t))
                                     (filter (fn [y] (not (contains? (set %) y)))
                                             (get indexes (last %)))) [(first (sort inputs))])))


(defn word-chains
  [inputs]
  (let [p (tree-paths inputs)]
    (filter #(= (count inputs) (count %)) p)))


(defn word-chain?
  [inputs]
  (not (empty? (word-chains inputs))))


(comment
  (fn [input]
    ;; Smooshed into one giant function/let block to make 4clojure text input happy
    (let [off-by-one-or-equal-length? (fn
                                        [x y]
                                        (<= (Math/abs (- (count x) (count y))) 1))
          substituted? (fn
                         [x y]
                         (= 1 (get (frequencies (map = x y)) false)))
          includes-all-chars? (fn
                                [s1 s2]
                                ;; Using String#contains because 4clojure doesn't load clojure.string
                                (= s2 (apply str (filter #(.contains s1 (str %)) s2))))
          connectable? (fn
                         [x y]
                         (cond
                           (not (off-by-one-or-equal-length? x y))
                           false
                           (= (count x) (count y))
                           (substituted? x y)
                           (< (count x) (count y))
                           (includes-all-chars? y x)
                           (> (count x) (count y))
                           (includes-all-chars? x y)))
          find-matches (fn
                         [item other-items]
                         ;; Apparently 4clojure's Clojure version doesn't know about some?
                         (let [matches (filter #(not (nil? %))
                                               (map #(when (connectable? item %) %)
                                                    (remove #(= item %)
                                                            other-items)))]
                           [item (vec matches)]))
          indexed (fn
                    [inputs]
                    (into {} (mapv #(find-matches % inputs) inputs)))
          tree-paths (fn
                       [inputs]
                       (let [indexes (indexed inputs)]
                         (tree-seq #(not (nil? (seq %))) #(map (fn [t] (conj % t))
                                                          (filter (fn [y] (not (contains? (set %) y)))
                                                                  (get indexes (last %)))) [(first (sort inputs))])))
          word-chains (fn
                        [inputs]
                        (let [p (tree-paths inputs)]
                          (filter #(= (count inputs) (count %)) p)))
          word-chain? (fn
                        [inputs]
                        (not (empty? (word-chains inputs))))]
      (word-chain? input))))

