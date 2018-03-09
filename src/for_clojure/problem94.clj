(ns for-clojure.problem94)

;
; Game of Life
; http://www.4clojure.com/problem/94
;
; Difficulty:	Hard
; Topics:	game
;
; The game of life is a cellular automaton devised by mathematician John Conway.
;
; The 'board' consists of both live (#) and dead ( ) cells. Each cell interacts with
; its eight neighbours (horizontal, vertical, diagonal), and its next state is dependent on the following rules:
;
;  1) Any live cell with fewer than two live neighbours dies, as if caused by under-population.
;  2) Any live cell with two or three live neighbours lives on to the next generation.
;  3) Any live cell with more than three live neighbours dies, as if by overcrowding.
;  4) Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
;
; Write a function that accepts a board, and returns a board representing the next generation of cells.
;


(def alive \#)


(def dead \space)


(def directions
  {:NW [-1 1]
   :N  [0 1]
   :NE [1 1]
   :E  [1 0]
   :SE [1 -1]
   :S  [0 -1]
   :SW [-1 -1]
   :W  [-1 0]})


(defn cell-state
  [board coords]
  (get-in board coords))


(defn alive?
  [board coords]
  (= alive (cell-state board coords)))


(defn neighbors
  [board coords]
  (filter #(some? (cell-state board %))
          (map
            #(map + coords (second %))
            directions)))


(defn count-live-neighbors
  [board coords]
  (let [neighbors (neighbors board coords)]
    (count (filter #(alive? board %) neighbors))))


(defn next-cell-state
  ;  1) Any live cell with fewer than two live neighbours dies, as if caused by under-population.
  ;  2) Any live cell with two or three live neighbours lives on to the next generation.
  ;  3) Any live cell with more than three live neighbours dies, as if by overcrowding.
  ;  4) Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
  [board coords]
  (let [num-live-neighbors (count-live-neighbors board coords)
        next-state (cond
                     (< num-live-neighbors 2)
                     dead
                     (contains? #{2 3} num-live-neighbors)
                     (if (alive? board coords)
                       alive
                       (if (= 3 num-live-neighbors)
                         alive
                         dead))
                     (> num-live-neighbors 3)
                     dead)]
    next-state))


(defn cell-updates
  [board]
  (into {}
        (for [x (range (count (first board)))
              y (range (count board))]
          [[x y] (next-cell-state board [x y])])))


(defn board-nextgen
  [board]
  (let [vboard (mapv vec board)
        cell-updates (cell-updates board)
        raw-next-board (reduce
                         (fn [current-board cell-update]
                           (let [coords (first cell-update)
                                 state (second cell-update)]
                             (assoc-in current-board coords state)))
                         vboard
                         cell-updates)]
    (mapv #(apply str %) raw-next-board)))


(defn run-tests
  []

  [(= (board-nextgen ["      "
                      " ##   "
                      " ##   "
                      "   ## "
                      "   ## "
                      "      "])
      ["      "
       " ##   "
       " #    "
       "    # "
       "   ## "
       "      "])

   (= (board-nextgen ["     "
                      "     "
                      " ### "
                      "     "
                      "     "])
      ["     "
       "  #  "
       "  #  "
       "  #  "
       "     "])

   (= (board-nextgen ["      "
                      "      "
                      "  ### "
                      " ###  "
                      "      "
                      "      "])
      ["      "
       "   #  "
       " #  # "
       " #  # "
       "  #   "
       "      "])])


(comment

  ;
  ; Everything jammed into a single function for the sake of 4Clojure's textarea
  ;
  (fn [board]
    (let [alive \#
          dead \space
          directions
          {:NW [-1 1]
           :N  [0 1]
           :NE [1 1]
           :E  [1 0]
           :SE [1 -1]
           :S  [0 -1]
           :SW [-1 -1]
           :W  [-1 0]}
          cell-state (fn
                       [board coords]
                       (get-in board coords))
          alive? (fn
                   [board coords]
                   (= alive (cell-state board coords)))
          ;
          ; 4Clojure throws up on some?
          ;
          neighbors (fn
                      [board coords]
                      (filter #(not (nil? (cell-state board %)))
                              (map
                                #(map + coords (second %))
                                directions)))
          count-live-neighbors (fn
                                 [board coords]
                                 (let [neighbors (neighbors board coords)]
                                   (count (filter #(alive? board %) neighbors))))
          next-cell-state (fn
                            ;  1) Any live cell with fewer than two live neighbours dies, as if caused by under-population.
                            ;  2) Any live cell with two or three live neighbours lives on to the next generation.
                            ;  3) Any live cell with more than three live neighbours dies, as if by overcrowding.
                            ;  4) Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
                            [board coords]
                            (let [num-live-neighbors (count-live-neighbors board coords)
                                  next-state (cond
                                               (< num-live-neighbors 2)
                                               dead
                                               (contains? #{2 3} num-live-neighbors)
                                               (if (alive? board coords)
                                                 alive
                                                 (if (= 3 num-live-neighbors)
                                                   alive
                                                   dead))
                                               (> num-live-neighbors 3)
                                               dead)]
                              next-state))
          cell-updates (fn
                         [board]
                         (into {}
                               (for [x (range (count (first board)))
                                     y (range (count board))]
                                 [[x y] (next-cell-state board [x y])])))
          board-nextgen (fn
                          [board]
                          (let [vboard (mapv vec board)
                                cell-updates (cell-updates board)
                                raw-next-board (reduce
                                                 (fn [current-board cell-update]
                                                   (let [coords (first cell-update)
                                                         state (second cell-update)]
                                                     (assoc-in current-board coords state)))
                                                 vboard
                                                 cell-updates)]
                            (mapv #(apply str %) raw-next-board)))]
      (board-nextgen board)))
  )
