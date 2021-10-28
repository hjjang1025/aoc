(ns aoc-2018-1
  (:require [clojure.string :as str]))

;;===========[Part 1]===========

;;file read
(def input-list
  (str/split-lines (slurp "src/input_2018_1.txt")))

;;sum of list
(reduce + (map #(Integer/parseInt %) input-list))

;;threading macro
(->> (slurp "src/input_2018_1.txt")
     str/split-lines
     (map #(Integer/parseInt %))
     (reduce +))
