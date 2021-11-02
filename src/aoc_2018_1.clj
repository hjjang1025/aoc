(ns aoc-2018-1
  (:require [clojure.string :as str]))

;===========[Part 1]===========

;file read
(def input-list
  (str/split-lines (slurp "resources/input_2018_1.txt")))

;sum of list
(reduce + (map #(Integer/parseInt %) input-list))

;threading macro
(->> (slurp "resources/input_2018_1.txt")
     str/split-lines
     (map #(Integer/parseInt %))
     (reduce +))


;===========[Part 2]===========
(def int-list (map #(Integer/parseInt %) input-list))

;; 1. 변경치들을 반복적으로 읽어들임
(def int-cycle (cycle int-list))

;; 2. 변경치의 누적값을 계산
;; 3. 누적값 중 처음으로 중복되는 값을 구하기
(loop [rest-cycle int-cycle
       frequency-set #{}
       last-frequency 0]
  (let [change (first rest-cycle)
        rest-cycle (rest rest-cycle)]
      (if (frequency-set last-frequency)
        last-frequency
        (recur rest-cycle
               (conj frequency-set last-frequency)
               (+ last-frequency change)))))





