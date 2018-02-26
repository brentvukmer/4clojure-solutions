(ns fourclojure-solutions.problem92
  (:require [clojure.string :as str]))

;
; Read Roman numerals
; http://www.4clojure.com/problem/92
;
; Difficulty:	Hard
; Topics:	strings math
;
; Roman numerals are easy to recognize, but not everyone knows all the rules necessary to work with them.
; Write a function to parse a Roman-numeral string and return the number it represents.
;
; You can assume that the input will be well-formed, in upper-case, and follow the subtractive principle.
; You don't need to handle any numbers greater than MMMCMXCIX (3999), the largest number representable
; with ordinary letters.
;


(def roman-subs
  {"IV", "IIII"
   "IX", "VIIII"
   "XL", "XXXX"
   "XC", "LXXXX"
   "CM", "DCCCC"})


(def roman-digits
  {"M", 1000
   "D", 500
   "C", 100
   "L", 50
   "X", 10
   "V", 5
   "I", 1})


(defn roman->arabic
  [input-str]
  (let [expanded-digits (reduce (fn [accum sub] (str/replace accum sub (roman-subs sub))) input-str (keys roman-subs))
        individual-digits (map str (seq expanded-digits))]
    (reduce + (map roman-digits individual-digits))))


(defn run-tests
  []
  [(= 14 (roman->arabic "XIV"))
   (= 827 (roman->arabic "DCCCXXVII"))
   (= 3999 (roman->arabic "MMMCMXCIX"))
   (= 48 (roman->arabic "XLVIII"))])

(comment
  (fn [input-str]
    (let [roman-subs
          {"IV", "IIII"
           "IX", "VIIII"
           "XL", "XXXX"
           "XC", "LXXXX"
           "CM", "DCCCC"}
          roman-digits
          {"M", 1000
           "D", 500
           "C", 100
           "L", 50
           "X", 10
           "V", 5
           "I", 1}
          expanded-digits (reduce (fn [accum sub] (str/replace accum sub (roman-subs sub))) input-str (keys roman-subs))
          individual-digits (map str (seq expanded-digits))]
      (reduce + (map roman-digits individual-digits)))))