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
(defn fabric-piece [{:keys [x y width height]}]
  (->> (for [xs (take width (drop x (range)))  ; `(1 2) (range x (+ x width))
             ys (take height (drop y (range)))]; `(3 4) (range y (+ y height))
         [xs ys])
       frequencies))

; 🔥 loop로 구현한 지난 날..
(comment
  (loop [i 0
         order (first input-list)
         orders (rest input-list)
         fabric-map {}]
    (if (= i (count input-list))
      (->> fabric-map
           (filter #(-> % val (> 1)))
           count)
      (recur (inc i)
             (first orders)
             (rest orders)
             (merge-with + fabric-map (fabric-piece order))))))

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

; 🔥 loop로 구현한 지난 날..
(def fabric-intersection-set-old
  (->> (loop [i 0
              order (first input-list)
              orders (rest input-list)
              fabric-map {}]
         (if (= i (count input-list))
           (->> fabric-map
                (filter #(-> % val (> 1)))
                (map key))
           (recur (inc i)
                  (first orders)
                  (rest orders)
                  (merge-with + fabric-map (fabric-piece order)))))
       set))

; Refactoring⭐ loop to reduce
(def fabric-intersection-set
  (->> input-list
       (reduce generate-fabric-map {})
       (filter #(-> % val (> 1)))
       (map key)
       set))

;fabric-intersection-set에 포함된 좌표가 없는 order를 출력

; 🔥 loop로 구현한 지난 날..
(comment
  (loop [i 0
         order (first input-list)
         orders (rest input-list)]
    (let [piece-set (fabric-piece-set order)]
      (if (empty? (set/intersection piece-set fabric-intersection-set))
        order ;=> {:i 1019, :x 436, :y 770, :width 14, :height 22}
        (recur (inc i)
               (first orders)
               (rest orders))))))


; Refactoring⭐ loop to filter
(defn not-in-intersection-set [order]
  (when (empty? (set/intersection (fabric-piece-set order) fabric-intersection-set))
    order))

(->> input-list
     (map not-in-intersection-set)
     (remove nil?))
