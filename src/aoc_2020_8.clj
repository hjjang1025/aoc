(ns aoc-2020-8
  (:require [clojure.string :as str]))

;===========[Part 1]===========
(defn parse-instruction [instruction]
  (let [[_ opr arg]
        (re-matches #"([a-z]+) ([+-][0-9]+)" instruction)]
    {:operation (keyword opr)
     :argument (Integer/parseInt arg)}))


(def boot-program (->> (slurp "resources/input_2020_8.txt")
                       str/split-lines
                       (map parse-instruction)))

(def initial-state
  {:flag :ready
   :idx-order [0]
   :accumulator 0})

; :flag type
; :ready
; :running
; :in-loop
; :terminate

(defn in-loop? [state next-idx]
  (->> (conj (state :idx-order) next-idx)
       frequencies
       (filter (fn [[_ v]] (> v 1)))
       seq))

(defn terminate? [next-idx size-of-instructions]
  (> next-idx
     (- size-of-instructions 1)))



(defn update-flag [state next-idx size-of-instructions]
  (cond (in-loop? state next-idx) :in-loop
        (terminate? next-idx size-of-instructions) :terminate
        :else :running))


(defn get-next-idx [state delta]
  (+ (last (state :idx-order))
     delta))


(defn update-idx-order [state step]
  (conj (state :idx-order)
        (get-next-idx state step)))


(defn update-state [instruction state size-of-instructions]
  (case (instruction :operation)
    :nop (assoc state :flag      (update-flag state
                                              (get-next-idx state 1)
                                              size-of-instructions)
                      :idx-order (update-idx-order state 1))

    :jmp (assoc state :flag      (update-flag state
                                              (get-next-idx state (instruction :argument))
                                              size-of-instructions)
                      :idx-order (update-idx-order state (instruction :argument)))

    :acc (assoc state :flag      (update-flag state
                                              (get-next-idx state 1)
                                              size-of-instructions)
                      :idx-order (update-idx-order state 1)
                      :accumulator (+ (state :accumulator)
                                      (instruction :argument)))))

;finite state machine..
(defn fsm [[instructions state]]
  [instructions
   (let [last-execution-order (last (state :idx-order))]
     (if (= :terminate (state :flag))
       state
       (update-state (nth instructions last-execution-order)
                     state
                     (count instructions))))])


(fsm [boot-program initial-state])


(def first-in-loop-state (->> (iterate fsm [boot-program initial-state])
                              (drop-while (fn [[_ state]] (not= :in-loop (state :flag))))
                              first
                              second))
(comment
  (->> first-in-loop-state
       :accumulator))



;===========[Part 2]===========

(defn generate-boot-programs
  "part1 에서 구한 first-in-loop-state 의 idx-order 를 활용해서
  jmp->nop 로 바꾼 program sequence를 생성"
  [original-boot-program state]
 (let [jmp-idxs (->> (state :idx-order)
                     (filter #(= :jmp
                                 ((nth original-boot-program %) :operation))))]
   (->> jmp-idxs
        (map #(update-in (vec original-boot-program) [% :operation] (fn [_] :nop))))))


(defn start-fsm [[program state]]
  (->> (iterate fsm [program state])
       (drop-while (fn [[_ state]] (not (#{:in-loop :terminate} (state :flag)))))
       first
       second))

(->> (generate-boot-programs boot-program first-in-loop-state)
     (map #(start-fsm [% initial-state]))
     (filter #(= :terminate (% :flag)))
     (map :accumulator))


