(ns aoc-2018-2
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

;===========[Part 1]===========

;file read
(def input-list
  (str/split-lines (slurp "src/input_2018_2.txt")))

(defn count-duplicate [times str]
  (count (filter (fn [[_ v]] (= v times)) (frequencies str))))


(*
  (count (filter pos-int? (map (partial count-duplicate 2) input-list)))
  (count (filter pos-int? (map (partial count-duplicate 3) input-list))))




;===========[Part 2]===========
; list to vector
(def ids
  (->> (slurp "src/input_2018_2.txt")
       str/split-lines
       vec))

;(common-between "abcde" "abcee") => abce
;(common-between "abcdefff" "abceeggg") => nil
(defn common-between [str-a str-b]
  (let [common-str (->> (map vector str-a str-b)
                        (filter (fn [[x y]] (= x y)))
                        (map first)
                        str/join)]
    (when (= (count common-str) (- (count str-a) 1))
      common-str)))

; 연산할 조합 리스트
; ["a" "b"] ["a" "b"] => (("a" "a") ("a" "b") ("b" "a") ("b" "b"))
(def ids-cartesian-product (combo/cartesian-product ids ids))

(comment
  (loop [[str-a str-b] (first ids-cartesian-product)
         rest-cartesian-product (rest ids-cartesian-product)]
    (if (common-between str-a str-b)
      (common-between str-a str-b)
      (recur (first rest-cartesian-product)
             (rest rest-cartesian-product)))))








