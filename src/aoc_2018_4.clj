(ns aoc-2018-4
  (:require [clojure.string :as str]))

;===========[Part 1]===========

(defn parse-guard-number [str]
  (when-let [guard (->> (re-matcher #"\d+" str)
                        re-find)]
    (Integer/parseInt guard)))



(defn parse-record [str]
  (let [[_ year month day hour minute guard]
        (re-matches #"\[([0-9]+)-([0-9]+)-([0-9]+) ([0-9]+):([0-9]+)\] (.*)" str)]
    {:year (Integer/parseInt year)
     :month (Integer/parseInt month)
     :day (Integer/parseInt day)
     :hour (Integer/parseInt hour)
     :minute (Integer/parseInt minute)
     :guard (parse-guard-number guard)}))

(def records
  (->> (slurp "resources/input_2018_4.txt")
       str/split-lines
       sort
       (map parse-record)))

;1.-----------------------
;record to logs
;({10  [10 16 38 53]}
; {10 [2 3 5 33]}
; {587 [10 16 38 53]})

;⭐ [logs last-guard] -> {:keys [logs last-guard]}

(defn generate-guard-minute-logs [{:keys [logs last-guard]} record]
  (if-let [new-guard (record :guard)] ;새 guard 등장하면
    {:logs (conj logs {new-guard []}) ;빈 벡터로 교체
     :last-guard new-guard}
    {:logs (conj (pop logs)
                 {last-guard (conj (flatten (vals (last logs)))
                                   (:minute record))})
     :last-guard last-guard}))

;2.-----------------------------------
;log-list to log-map (같은 guard 병합) ⭐apply 함수로 사용

;({10  [10 16 38 53]}
; {10 [2 3 5 33]}
; {587 [10 16 38 53]})
; =>
;{10 [10 16 38 53 2 3 5 33]}
; 587 [10 16 38 53]}

;3---------------------------
;minute -> range list
;[10 16 38 53 2 3 5 33]  =>  (10, 11, 12, 13, .., 15, 38, 39, ... 52, 2, 5, 6, 7, ... 32)


(defn all-sleep-minutes
  "minute -> range list
  (all-minutes-sleep [10 16 38 53 2 3 5 33])
  => [10 16 38 53 2 3 5 33]  =>  (10, 11, 12, 13, .., 15, 38, 39, ... 52, 2, 5, 6, 7, ... 32)
  "
  [repeat-sleep-wake]
  (->> repeat-sleep-wake
       (partition 2)
       (map (fn [[sleep wake]] (range sleep wake))) ;⭐ fn 추가해서 range 적용
       (reduce concat)))

;4-------------------------------
(defn most-frequent-minute-and-frequency [minutes]
  (if (seq minutes)
    (->> (frequencies minutes)
         (apply max-key val))
    [0 0]))


(defn format-guard-sleep-log
  "get duration and frequencies"
  [[guard repeat-sleep-wake]]
  (let [sleep-minutes (all-sleep-minutes repeat-sleep-wake)
        [most-frequent-minute max-frequency] (most-frequent-minute-and-frequency sleep-minutes)]
    {:guard                guard
     :duration             (count sleep-minutes)
     :most-frequent-minute most-frequent-minute
     :max-frequency        max-frequency}))


(defn generate-guard-sleep-frequencies-log [records]
  (->> records
       (reduce generate-guard-minute-logs {:logs [] :last-guard 0}) ;1
       :logs
       (apply (partial merge-with into)) ;2 ⭐ apply 사용 (같은 guard 병합)
       (map format-guard-sleep-log))) ;3, 4

(defn print-answer
  "⭐ 정답 계산 "
  [{:keys [guard most-frequent-minute]}]
  (* guard most-frequent-minute))

(comment
  (->> records
       generate-guard-sleep-frequencies-log
       (apply max-key :duration)  ;⭐ 최대값 구하기->sort 대신 max-key로 변경
       print-answer))

;===========[Part 2]===========
(comment
  (->> records
       generate-guard-sleep-frequencies-log
       (filter #(-> % :minute-frequencies seq)) ; NullPointException을 막기 위해 한번도 잠들지 않은 guard를 filter로 제거
       (apply max-key #(-> % :minute-frequencies last second)) ; 가장 큰 frequency 기준으로 최대값 추출
       print-answer))
