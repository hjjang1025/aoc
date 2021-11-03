(ns aoc-2018-5
  (:require [clojure.string :as str]))

;===========[Part 1]===========

(defn abs "절대값" [n] (max n (- n)))

;(removable? \a \A)
;=> true
(defn removable? "제거할 수 있는 조건" [left right]
  (= 32 (abs (- (int left) (int right)))))

(defn generate-remain-polymer [remain polymer]
  (if (and (seq remain) ;NullPointException 방지
           (removable? (last remain) polymer))
    (vec (drop-last remain))
    (conj remain polymer)))

(comment
  (->> (slurp "resources/input_2018_5.txt")
       (reduce generate-remain-polymer [])
       count))



