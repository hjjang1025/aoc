(ns aoc-2018-4
  (:require [clojure.string :as str]))

;===========[Part 1]===========
;Usage
;(match "[1518-11-01 00:00] Guard #10 begins shift")
(defn match [str]
  (let [[_ year month day hour minute action]
        (re-matches #"\[([0-9]+)-([0-9]+)-([0-9]+) ([0-9]+):([0-9]+)\] (.*)" str)]
    {:year (Integer/parseInt year)
     :month (Integer/parseInt month)
     :day (Integer/parseInt day)
     :hour (Integer/parseInt hour)
     :minute (Integer/parseInt minute)
     :action action}))

(def input-list
  (->> (slurp "resources/input_2018_4.txt")
       str/split-lines
       sort
       (map match)))


;Usage
;(match-guard {:year 1518, :month 10, :day 29, :hour 23, :minute 52, :action "Guard #1601 begins shift"})
(defn match-guard [{:keys [action]}]
  (->> (re-matcher #"\d+" action)
       re-find))


;1.-----------------------
;record to logs
;({10  [10 16 38 53]}
; {10 [2 3 5 33]}
; {587 [10 16 38 53]})

;Refactoring⭐ [logs last-guard] -> {:keys [logs last-guard]}

(defn generate-logs [{:keys [logs last-guard]} record]
  (if-let [new-guard (match-guard record)]
    {:logs (conj logs {new-guard []})
     :last-guard new-guard}
    {:logs (conj (pop logs)
                 {last-guard (conj (first (vals (last logs))) (:minute record))})
     :last-guard last-guard}))

;2.-----------------------------------
;log-list to log-map (같은 guard 병합) ⭐apply 함수로 사용(line:92)

;({10  [10 16 38 53]}
; {10 [2 3 5 33]}
; {587 [10 16 38 53]})
; =>
;{10 [10 16 38 53 2 3 5 33]}
; 587 [10 16 38 53]}

;3---------------------------
;minute -> range list
;[10 16 38 53 2 3 5 33]  =>  (10, 11, 12, 13, .., 15, 38, 39, ... 52, 2, 5, 6, 7, ... 32)

;Usage
;(log-to-range [10 16 38 53 2 3 5 33])
(defn log-to-range [vec]
  (->> vec
       (partition 2)
       (map (fn [[sleep wake]] (range sleep wake))) ;Refactoring⭐ fn 추가해서 range 적용
       (reduce concat)))

;4-------------------------------
;get duration (count list) and frequencies
(defn range-to-info "get duration and frequencies" [guard list]
  {:guard guard
   :duration (count list)
   :frequencies (sort-by val (frequencies list))})


(comment
  (->> input-list
       (reduce generate-logs {:logs [] :last-guard 0}) ;1
       :logs
       (apply (partial merge-with into)) ;2 Refactoring⭐ 불필요한 함수 삭제 후 apply 사용
       (map (fn [[k v]] (range-to-info k (log-to-range v))))  ;3, 4
       (apply max-key :duration))) ;Refactoring⭐ 최대값 구하기->sort 대신 max-key로 변경


;===========[Part 2]===========
(comment
  (->> input-list
       (reduce generate-logs {:logs [] :last-guard 0})
       :logs
       (apply (partial merge-with into))
       (map (fn [[k v]] (range-to-info k (log-to-range v)))) ;--여기까지 Part 1과 동일합니다--
       (filter #(->> % :frequencies seq)) ; NullPointException을 막기 위해 한번도 잠들지 않은 guard를 filter로 제거
       (apply max-key #(->> % :frequencies last second)))) ; 가장 큰 frequency 기준으로 최대값 추출
