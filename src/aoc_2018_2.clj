(ns aoc-2018-2
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

;===========[Part 1]===========

;file read
(def input-list
  (str/split-lines (slurp "resources/input_2018_2.txt")))

;[⭐1]️ frequencies 를 count-times-frequencies 밖에서 계산
(defn count-times-frequencies [times frequencies]
  (count
    (filter (fn [[_ v]] (= v times))
            frequencies)))

;[⭐2] 문제의 정답을 계산할 때 반복되는 부분을 함수로 분리
(defn count-duplicate [times seq]
  (->> seq
       (map frequencies) ;[⭐1]
       (map (partial count-times-frequencies times))
       (filter pos-int?)
       count))

;[⭐3] ->> 로 정리
(comment
  (* (->> input-list (count-duplicate 2))
     (->> input-list (count-duplicate 3))))



;===========[Part 2]===========
; list to vector
(def ids
  (->> (slurp "resources/input_2018_2.txt")
       str/split-lines
       vec))

;(common-between "abcde" "abcee") => abce
;(common-between "abcdefff" "abceeggg") => nil
(defn common-between [str-a str-b]
  (let [common-str (->> (map vector str-a str-b) ; [(\a \a) (\b \b) (\c \c) (\d \e) (\e \e)]
                        (filter (fn [[x y]] (= x y))) ; [(\a \a) (\b \b) (\c \c) (\e \e)]
                        (map first) ; [\a \b \c \e]
                        str/join)]  ; abce
    (when (= (count common-str) (- (count str-a) 1))
      common-str)))

; 연산할 조합 리스트
; ["a" "b" "c"] => (("a" "b") ("a" "c") ("b" "c"))
;Refactoring⭐️ combinations 사용
(def ids-cartesian-product (combo/combinations ids 2))

;🔥loop
(comment
  (loop [[str-a str-b] (first ids-cartesian-product)
         rest-cartesian-product (rest ids-cartesian-product)]
    (if (common-between str-a str-b)
      (common-between str-a str-b)
      (recur (first rest-cartesian-product)
             (rest rest-cartesian-product)))))

;⭐️Reduce
(comment
  (->> ids-cartesian-product
       (keep #(apply common-between %))))

