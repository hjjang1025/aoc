(ns aoc-2018-5
  (:require [clojure.string :as str]))

;===========[Part 1]===========
;(reactive?? \a \A) => true
;(reactive?? \A \a) => true
(defn reactive? "반응할 수 있는 조건" [left right]
  (= 32 (Math/abs (- (int left) (int right)))))

;reactive? 에 부합하지 않는 문자열 vector(=remain)를 취합
;
;example : vVabcZz
;v => [v]
;vV => []
;vVa => [a]
;vVab => [a b]
;vVabc => [a b c]
;vVabcZ => [a b c Z]
;vVabcZz => [a b c]
(defn generate-remain-polymer [remain unit]
  (if (and (seq remain) ;NullPointException 방지
           (reactive? (last remain) unit))
    (vec (drop-last remain)) ;conj로 unit을 추가할 때 last에 추가하기 위해 vector로 사용
    (conj remain unit)))

;속도가 너무 느려서 자료구조 고민을..
;(defn generate-remain-map [{:keys [remain remain-count last-unit]} unit]
;  (if (and (> remain-count 0)
;           (reactive? last-unit unit))
;    {:remain :remain-count (dec remain-count) :last-unit last-unit}
;    {(conj remain unit) :remain-count (inc remain-count) :last-unit unit}))
;

(def polymer (slurp "resources/input_2018_5.txt"))

(comment
  (->> polymer
       (reduce generate-remain-polymer [])
       count))

;(comment
;  (->> polymer
;       (reduce generate-remain-map {:remain [] :remain-count 0 :last-unit nil})))

;===========[Part 2]===========
;알파벳 하나씩 빠진 polymer sequence

(def units `("aA" "bB" "cC" "dD" "eE" "fF" "gG" "hH" "iI" "jJ" "kK" "lL" "mM" "nN"
             "oO" "pP" "qQ" "rR" "sS" "tT" "uU" "vV" "wW" "xX" "yY" "zZ"))

(defn polymer-without-unit [unit]
  (str/replace polymer
               (re-pattern (format "[%s]" unit))
               ""))

(defn get-remain-count [str]
  (->> str
       (reduce generate-remain-polymer [])
       count))

(comment
  (->> units
       (map polymer-without-unit)
       (map get-remain-count)))



