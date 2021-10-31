(ns aoc-2018-3
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

;===========[Part 1]===========
; "#1 @ 935,649: 22x22"
; => {:i 1, :x 935, :y 649, :width 22, :height 22}
(defn match [str]
  (let [[_ i x y w h] (re-matches #"#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)" str)]
    {:i (Integer/parseInt i)
     :x (Integer/parseInt x)
     :y (Integer/parseInt y)
     :width (Integer/parseInt w)
     :height (Integer/parseInt h)}))

;file read
(def input-list
  (->> (slurp "src/input_2018_3.txt")
       str/split-lines
       (map match)))

; {:x 1, :y 3, :width 2, :height 2}
; => {[1 3] 1 [1 4] 1
;     [2 3] 1 [2 4] 1}
(defn fabric-piece [{:keys [x y width height]}]
  (->> (for [xs (take width (drop x (range)))  ; `(1 2)
             ys (take height (drop y (range)))]; `(3 4)
         [xs ys])
       frequencies))

(loop [i 0
       order (first input-list)
       rest-order (rest input-list)
       fabric-map {}]
  (if (= i (count input-list))
    (->> fabric-map
         (filter #(-> % val (> 1)))
         count)
    (do
      (recur (inc i)
             (first rest-order)
             (rest rest-order)
             (merge-with + fabric-map (fabric-piece order))))))


;===========[Part 2]===========