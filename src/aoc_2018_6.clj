(ns aoc-2018-6
  (:require [clojure.string :as str]))

;===========[Part 1]===========
(defn parse-coordinate [str]
  (let [[_ x y]
        (re-matches #"([0-9]+), ([0-9]+)" str)]
    [(Integer/parseInt x) (Integer/parseInt y)]))

(def chronal-coordinates
  (->> (slurp "resources/input_2018_6.txt")
       str/split-lines
       (map parse-coordinate)))

(defn area
  "Processing 1
  area : 좌표 최대값, 최소값으로 추출한 좌표 sequence"
  [coordinates]
  (let [xs (map first coordinates)
        ys (map second coordinates)
        min-x (apply min xs)
        min-y (apply min ys)
        max-x (apply max xs)
        max-y (apply max ys)]
    (for [xs (range min-x (+ max-x 1))
          ys (range min-y (+ max-y 1))]
      [xs ys])))


(defn manhattan-distance [[a b] [c d]]
  (+ (Math/abs (- a c))
     (Math/abs (- b d))))

(defn index-of-min-distance
  "
  Processing 2
  index-of-min-distance
  (0 5 9 5 8 15) => 0
  (1 1 9 5 8 15) => nil ;맨하튼 거리가 같은 좌표가 2개 이상
  (11 6 6 6 3 4) => 4
  "
  [distance-list]
  (let [min-distance (apply min distance-list)]
    (when (= 1 (get (frequencies distance-list) min-distance))
      (.indexOf distance-list min-distance))))

(defn index-of-closest-coordinate [coordinates coordinate]
  (->> coordinates
       (map (partial manhattan-distance coordinate))
       index-of-min-distance))

(defn area-boundary
  "area의 가장자리에 있는 좌표 sequence"
  [coordinates]
  (let [xs (map first coordinates)
        ys (map second coordinates)
        min-x (apply min xs)
        min-y (apply min ys)
        max-x (apply max xs)
        max-y (apply max ys)]
    (->> (for [xs (range min-x (+ max-x 1))
               ys (range min-y (+ max-y 1))]
           (when (or (= xs min-x)
                     (= ys min-y)
                     (= xs max-x)
                     (= ys max-y)) [xs ys]))
         (keep identity))))

(def infinite-coordinates
  "무한 확장하는 좌표 set"
  (->> chronal-coordinates
       area-boundary
       (map (partial index-of-closest-coordinate chronal-coordinates))
       (keep identity) ;nil 제거
       set))

(comment
  (->> chronal-coordinates ;Parse
       area ;Processing 1
       (map (partial index-of-closest-coordinate chronal-coordinates)) ;Processing2
       frequencies ;Aggregation
       (filter #(->> % key infinite-coordinates not)) ;무한한 좌표를 제거하고 return [index-of-coordinate 면적]
       (apply max-key second))) ;Print : 면적 기준으로 최고값 Print

;===========[Part 2]===========

(defn within-desired-region? [coordinates coordinate]
  (let [sum-of-distance (->> coordinates
                             (map (partial manhattan-distance coordinate))
                             (reduce +))]
    (< sum-of-distance 10000)))

(comment
  (->> chronal-coordinates
       area
       (map (partial within-desired-region? chronal-coordinates))
       (filter true?)
       count))
