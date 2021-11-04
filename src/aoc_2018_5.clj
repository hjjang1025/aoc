(ns aoc-2018-5
  (:require [clojure.string :as str]))

;===========[Part 1]===========
;(reactive? \a \A) => true
;(reactive? \A \a) => true
(defn reactive? "반응할 수 있는 조건" [left right]
  (= 32 (Math/abs (- (int left) (int right)))))

;example : vVabcZz
;unit => remain
;v => [v]
;V => []
;a => [a]
;b => [a b]
;c => [a b c]
;Z => [a b c Z]
;z => [a b c]
(defn generate-remain-polymer
  "reactive? 에 부합하지 않는 문자열 vector(=remain)를 취합"
  [remain unit]
  (if (and (seq remain) ;NullPointException 방지
           (reactive? (peek remain) unit)) ;🌟성능개선!🌟last -> peek
    (vec (pop remain)) ;🌟성능개선!🌟drop-last -> pop
    (conj remain unit)))

(def polymer (slurp "resources/input_2018_5.txt"))

(comment
  (->> polymer
       (reduce generate-remain-polymer [])
       count))

;===========[Part 2]===========


(def units ["aA" "bB" "cC" "dD" "eE" "fF" "gG" "hH" "iI" "jJ" "kK" "lL" "mM" "nN"
                "oO" "pP" "qQ" "rR" "sS" "tT" "uU" "vV" "wW" "xX" "yY" "zZ"])
;TODO for -> get seq
;(for [i (range 97 123)] (reduce..))
;regex 알파벳 무관하게

(defn polymer-without-unit
  "unit(알파벳) 하나씩 빠진 polymer sequence"
  [unit]
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
       (map get-remain-count)
       (apply min)))



