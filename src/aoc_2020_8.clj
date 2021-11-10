(ns aoc-2020-8
  (:require [clojure.string :as str]))

;===========[Part 1]===========
(defn parse-instruction [instruction]
  (let [[_ opr arg]
        (re-matches #"([a-z]+) ([+-][0-9]+)" instruction)]
    {:operation (keyword opr)
     :argument (Integer/parseInt arg)}))


(def instructions (->> (slurp "resources/input_2020_8.txt")
                      str/split-lines
                      (map parse-instruction)))

(def initial-state
  {:flag :ready
   :execution-idx-order [0]
   :accumulator 0})

; :flag type
; :ready
; :running
; :in-loop
; :done

(defn in-loop? [state execution-idx]
  (->> (conj (state :execution-idx-order) execution-idx)
       frequencies
       (filter (fn [[_ v]] (> v 1)))
       seq))

(defn done? [execution-idx size-of-instructions]
  (>= execution-idx
     (- size-of-instructions 1)))



(defn update-flag [state execution-idx size-of-instructions]
  (cond (in-loop? state execution-idx) :in-loop
        (done? execution-idx size-of-instructions) :done
        :else :running))

(defn update-execution-order [state step]
  (assoc state :execution-idx-order
               (conj (state :execution-idx-order)
                     (+ (last (state :execution-idx-order))
                        step))))

(defn get-execution-idx [state delta]
  (+ (last (state :execution-idx-order))
     delta))


(defn update-state [instruction state size-of-instructions]
  (case (instruction :operation)
    :nop (assoc state :flag (update-flag state
                                         (get-execution-idx state 1)
                                         size-of-instructions)
                      :execution-idx-order (conj (state :execution-idx-order)
                                                 (get-execution-idx state 1))) ;execution-order +1
    :jmp (assoc state :flag (update-flag state
                                         (get-execution-idx state (instruction :argument))
                                         size-of-instructions)
                      :execution-idx-order (conj (state :execution-idx-order)
                                                 (get-execution-idx state (instruction :argument)))) ;execution-order +jmp
    :acc (assoc state :flag (update-flag state
                                         (get-execution-idx state 1)
                                         size-of-instructions)
                      :execution-idx-order (conj (state :execution-idx-order)
                                                 (get-execution-idx state 1))
                      :accumulator (+ (state :accumulator)
                                      (instruction :argument)))))


(defn fsm [[instructions state]]
  [instructions
   (let [last-execution-order (last (state :execution-idx-order))]
       (update-state (nth instructions last-execution-order)
                     state
                     (count instructions)))])





(fsm [instructions initial-state])

(comment
  (->> (iterate fsm [instructions initial-state])
       (drop-while (fn [[_ state]] (not= :in-loop (state :flag))))
       first
       second
       :accumulator))



;===========[Part 2]===========


