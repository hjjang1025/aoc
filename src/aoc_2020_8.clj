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
  {:type :ready
   :idx-order [0]
   :accumulator 0})

; [state type]
; :ready
; :running
; :in-loop
; :terminate

(defn in-loop? [state next-idx]
  ((set (state :idx-order)) next-idx))


(defn terminate? [next-idx len-of-program]
  (> next-idx
     (- len-of-program 1)))


(defn update-type [state next-idx len-of-program]
  (cond (in-loop? state next-idx) :in-loop
        (terminate? next-idx len-of-program) :terminate
        :else :running))


(defn get-next-idx [state delta]
  (+ (last (state :idx-order))
     delta))


(defn update-idx-order [state delta]
  (conj (state :idx-order)
        (get-next-idx state delta)))


(defn update-state [instruction state len-of-program]
  (case (instruction :operation)
    :nop (assoc state :type      (update-type state
                                              (get-next-idx state 1)
                                              len-of-program)
                      :idx-order (update-idx-order state 1))

    :jmp (assoc state :type      (update-type state
                                              (get-next-idx state (instruction :argument))
                                              len-of-program)
                      :idx-order (update-idx-order state (instruction :argument)))

    :acc (assoc state :type      (update-type state
                                              (get-next-idx state 1)
                                              len-of-program)
                      :idx-order (update-idx-order state 1)
                      :accumulator (+ (state :accumulator)
                                      (instruction :argument)))))


(defn run-program [[instructions state]]
  [instructions
   (let [last-execution-order (last (state :idx-order))
         len-of-program (count instructions)]
     (if (= :terminate (state :type))
       state
       (update-state (nth instructions last-execution-order)
                     state
                     len-of-program)))])


(defn start-program [[program state]]
  (->> (iterate run-program [program state])
       (drop-while (fn [[_ state]] (not (#{:in-loop :terminate} (state :type)))))
       first
       second))

(def first-in-loop-state (start-program [boot-program initial-state]))

(comment
  (->> first-in-loop-state
       :accumulator))



;===========[Part 2]===========

(defn terminable-boot-programs
  "part1 에서 구한 first-in-loop-state 의 idx-order 를 활용해서
  jmp->nop 로 바꾼 program sequence를 생성"
  [original-boot-program state]
 (let [jmp-idxs (->> (state :idx-order)
                     (filter #(= :jmp
                                 ((nth original-boot-program %) :operation))))]
   (->> jmp-idxs
        (map #(update-in (vec original-boot-program)
                         [% :operation]
                         (fn [_] :nop))))))

(comment
  (->> (terminable-boot-programs boot-program first-in-loop-state)
       (map #(start-program [% initial-state]))
       (filter #(= :terminate (% :type)))
       (map :accumulator)))


