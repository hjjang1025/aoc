(ns aoc-2018-4
  (:require [clojure.string :as str]))

;===========[Part 1]===========
;[1518-11-01 00:00] Guard #10 begins shift

(defn match [str]
  (let [[_ year month day hour minute action]
        (re-matches #"\[([0-9]+)-([0-9]+)-([0-9]+) ([0-9]+):([0-9]+)\] (.*)" str)]
    {:year (Integer/parseInt year)
     :month (Integer/parseInt month)
     :day (Integer/parseInt day)
     :hour (Integer/parseInt hour)
     :minute (Integer/parseInt minute)
     :action action}))

(match "[1518-11-01 00:00] Guard #10 begins shift")


(def input-list
  (->> (slurp "src/input_2018_4.txt")
       str/split-lines
       sort
       (map match)))


(defn match-guard [{:keys [action]}]
  (->> (re-matcher #"\d+" action)
       re-find))

(match-guard {:year 1518, :month 10, :day 29, :hour 23, :minute 52, :action "Guard #1601 begins shift"})

;1.-----------------------
;record to logs
;({10  [10 16 38 53]}
; {10 [2 3 5 33]}
; {587 [10 16 38 53]})

(defn generate-logs [[logs last-guard] record]
  (if-let [new-guard (match-guard record)]
    [(conj logs {new-guard []}) new-guard]
    [(conj (pop logs)
           {last-guard (conj (first (vals (last logs))) (:minute record))})
     last-guard]))

;2.-----------------------------------
;log-list to log-map (같은 guard 병합)

;({10  [10 16 38 53]}
; {10 [2 3 5 33]}
; {587 [10 16 38 53]})
; =>
;{10 [10 16 38 53 2 3 5 33]}
; 587 [10 16 38 53]}
(defn merge-with-seq [x y]
  (merge-with into x y))

;3---------------------------
;minute -> range list
;[10 16 38 53 2 3 5 33]  =>  (10, 11, 12, 13, .., 15, 38, 39, ... 52, 2, 5, 6, 7, ... 32)
(defn log-to-range [vec]
  (->> vec
       (partition 2)
       (map #(range (first %) (second %)))
       (reduce concat)))

(log-to-range [10 16 38 53 2 3 5 33])

;4-------------------------------
;get duration (count list)
;get frequencies
(defn range-to-info [guard list]
  {:guard guard
   :duration (count list)
   :frequencies (sort-by val (frequencies list))})


(->> input-list
     (reduce generate-logs [[] 0]) ;1
     first
     (reduce merge-with-seq) ;2
     (map (fn [[k v]] (range-to-info k (log-to-range v))))  ;3, 4
     (sort-by :duration)
     last)
