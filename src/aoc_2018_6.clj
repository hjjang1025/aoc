(ns aoc-2018-6
  (:require [clojure.string :as str]))

;===========[Part 1]===========
(defn parse-coordinate [str]
  (let [[_ x y]
        (re-matches #"([0-9]+), ([0-9]+)" str)]
    [(Integer/parseInt x) (Integer/parseInt y)]))

(defn manhattan-distance [[a b] [c d]]
  (+ (Math/abs (- a c))
     (Math/abs (- b d))))

(manhattan-distance [1 4] [5 8])

;parse
#_(def chronal-coordinates
    (->> (slurp "resources/input_2018_6.txt")
         str/split-lines
         (map parse-coordinate)))

(def chronal-coordinates [[1 1] [1 6] [8 3] [3 4] [5 5] [8 9]])

;processing 1 generate area
(defn area [coordinates]
  (let [xs (map first coordinates)
        ys (map second coordinates)
        min-x (apply min xs)
        min-y (apply min ys)
        max-x (apply max xs)
        max-y (apply max ys)]
    (for [xs (range min-x (+ max-x 1))
          ys (range min-y (+ max-y 1))]
      [xs ys])))

;processing 2
;area -> manhattan-distance

;processing 3
; index-of-min-distance
; (0 5 9 5 8 15) => 0
; (1 1 9 5 8 15) => nil ;두 좌표 이상 거리가 같다
; (11 6 6 6 3 4) => 4

(defn index-of-min-distance [distance-list]
  (let [min-distance (apply min distance-list)]
    (when (= 1 (get (frequencies distance-list) min-distance))
      (.indexOf distance-list min-distance))))

(index-of-min-distance '(0 5 9 5 8 15))
(index-of-min-distance '(1 1 9 5 8 15))
(index-of-min-distance '(11 6 6 6 3 4))


(defn manhattan-distance-list [coordinate]
  (->> chronal-coordinates
       (map (partial manhattan-distance coordinate))))


(manhattan-distance-list [3 3])


(comment
  (->> chronal-coordinates
       area
       (map manhattan-distance-list)
       (map index-of-min-distance)
       frequencies
       (sort-by val)))

