(ns aoc-2018-1
  (:require [clojure.string :as str]))

;;[Part 1]
;;file read
(def input_list
  (str/split-lines (slurp "src/input_2018_1.txt")))

;;string -> int
(defn toInt [str]
  (if (nil? str) 0 (Integer/parseInt str) ))

;;sum of list
(reduce + (map toInt input_list))

