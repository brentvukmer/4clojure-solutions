(ns for-clojure.problem86)


; Happy numbers are positive integers that follow a particular formula:
; take each individual digit, square it, and then sum the squares to get a new number.
; Repeat with the new number and eventually, you might get to a number whose squared sum is 1.
;
; This is a happy number.
;
; An unhappy number (or sad number) is one that loops endlessly.
;
; Write a function that determines if a number is happy or not.

(comment
  (= (__ 7) true)
  (= (__ 986543210) true)
  (= (__ 2) false)
  (= (__ 3) false))


(defn digits
  [num]
  (map #(Integer/parseInt (str %)) (seq (str num))))


(defn squares-sum
  [num]
  (->> (digits num)
       (map #(* % %))
       (reduce +)))

(defn happy?
  [num]
  (= 1 (last (take 1000 (iterate squares-sum num)))))

(comment
  (fn
    [num]
    (let [digits (fn
                   [num]
                   (map #(Integer/parseInt (str %)) (seq (str num))))
          squares-sum (fn
                        [num]
                        (->> (digits num)
                             (map #(* % %))
                             (reduce +)))
          happy? (fn
                   [num]
                   (= 1 (last (take 1000 (iterate squares-sum num)))))]
      (happy? num))))