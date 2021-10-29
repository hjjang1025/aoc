(ns aoc-2018-2
  (:require [clojure.string :as str]))

;===========[Part 1]===========

;file read
(def input-list
  (str/split-lines (slurp "src/input_2018_2.txt")))

(defn count-duplicate [times str]
  (count (filter (fn [[k v]] (= v times)) (frequencies str)) )
  )

(*
  (count (filter pos-int? (map (partial count-duplicate 2) input-list)))
  (count (filter pos-int? (map (partial count-duplicate 3) input-list)))
  )

