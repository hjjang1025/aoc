(ns aoc-2018-3
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;===========[Part 1]===========
; "#1 @ 935,649: 22x22"
; => {:i 1, :x 935, :y 649, :width 22, :height 22}
(defn match [str]
  (let [[_ i x y w h]
        (re-matches #"#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)" str)]
    {:i (Integer/parseInt i)
     :x (Integer/parseInt x)
     :y (Integer/parseInt y)
     :width (Integer/parseInt w)
     :height (Integer/parseInt h)}))

;file read
(def input-list
  (->> (slurp "resources/input_2018_3.txt")
       str/split-lines
       (map match)))

; {:x 1, :y 3, :width 2, :height 2}
; => {[1 3] 1 [1 4] 1
;     [2 3] 1 [2 4] 1}

;Refactoring⭐ take, drop to range
;(take width (drop x (range) 에서
;x가 아주 커질 경우 성능 이슈가 있을 수 있다고 합니다
(defn fabric-piece [{:keys [x y width height]}]
  (->> (for [xs (range x (+ x width))  ; `(1 2) ;Refactoring⭐ take, drop to range
             ys (range y (+ y height))]; `(3 4) ;Refactoring⭐ take, drop to range
         [xs ys])
       frequencies))



; Refactoring⭐ loop to reduce
;{[1 3] 1 [1 4] 1}
; [2 3] 1 [2 4] 1}
(defn generate-fabric-map [fabric-map order]
  (merge-with + fabric-map (fabric-piece order)))

(comment
  (->> input-list
       (reduce generate-fabric-map {})
       (filter #(-> % val (> 1)))
       count))



;===========[Part 2]===========
(defn fabric-piece-set [{:keys [x y width height]}]
  (->> (for [xs (take width (drop x (range)))  ; `(1 2)
             ys (take height (drop y (range)))]; `(3 4)
         [xs ys])
       set))

;fabric-intersection-set : part1의 마지막 loop를 활용한 교집합 좌표 set

; Refactoring⭐ loop to reduce
(def fabric-intersection-set
  (->> input-list
       (reduce generate-fabric-map {})
       (filter #(-> % val (> 1)))
       (map key)
       set))

;fabric-intersection-set에 포함된 좌표가 없는 order를 출력

; Refactoring⭐ loop to filter
(defn not-in-intersection-set [order]
  (when (empty? (set/intersection (fabric-piece-set order)
                                  fabric-intersection-set))
    order))

(comment
  (->> input-list
       (keep not-in-intersection-set))) ;Refactoring⭐ nil 제외하는 keep




