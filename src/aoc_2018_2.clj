(ns aoc-2018-2
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

;===========[Part 1]===========

;file read
(def box-ids
  (str/split-lines (slurp "resources/input_2018_2.txt")))


;[⭐1]️ frequencies 를 count-times-frequencies 밖에서 계산
(defn count-times-frequencies [times alphabet-frequencies]
  (->> alphabet-frequencies
       (filter (fn [[_ v]] (= v times)))
       count))


;[⭐2] 문제의 정답을 계산할 때 반복되는 부분을 함수로 분리
(defn count-duplicate-alphabet [times box-id]
  (->> box-id
       (map frequencies) ;[⭐1]
       (map (partial count-times-frequencies times))
       (filter pos-int?)
       count))

;[⭐3] ->> 로 정리
(comment
  (* (->> box-ids (count-duplicate-alphabet 2))
     (->> box-ids (count-duplicate-alphabet 3))))

;parse / process / aggregate / print

;===========[Part 2]===========
; list to vector
(def ids
  (->> (slurp "resources/input_2018_2.txt")
       str/split-lines
       vec))


(defn common-letters-between
  "(common-letters-between \"abcde\" \"abcee\") => abce"
  [str-a str-b]
  (->> (map vector str-a str-b) ; [(\a \a) (\b \b) (\c \c) (\d \e) (\e \e)]
       (filter (fn [[x y]] (= x y))) ; [(\a \a) (\b \b) (\c \c) (\e \e)]
       (map first) ; [\a \b \c \e]
       str/join))

(defn valid-common-letters [[str-a str-b]]
  (let [common-letters (common-letters-between str-a str-b)]
    (when (= (count common-letters)
             (- (count str-a) 1))
      common-letters)))


;;;;
; 연산할 조합 리스트
; ["a" "b" "c"] => (("a" "b") ("a" "c") ("b" "c"))
;⭐️ combinations 사용
(def ids-cartesian-product (combo/combinations ids 2))

;⭐️Reduce
(comment
  (->> ids-cartesian-product
       (keep valid-common-letters)))

