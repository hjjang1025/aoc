(ns aoc-2018-7
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;===========[Part 1]===========
(defn parse-step-edge [str]
  (let [[_ prev next]
        (re-matches #"Step ([A-Z]+) must be finished before step ([A-Z]+) can begin." str)]
    [prev next]))

;parsing 1 : edges
(def step-edges
  (->> (slurp "resources/input_2018_7.txt")
       str/split-lines
       (map parse-step-edge)))

;진입차수(in-degree)
;      A B C D E F
;(C A) 1 . 0 . . .
;(C F) 1 . 0 . . 1
;(A B) 1 2 0 . . 1
;(A D) 1 2 0 2 . 1
;(B E) 1 2 0 . 3 1
;(D E) 1 2 0 2 3 1
;(F E) 1 2 0 2 3 1

;sample of in-degrees
;[{:step "A", :in-degree 1}
; {:step "B", :in-degree 2}
; {:step "C", :in-degree 0}
; {:step "D", :in-degree 2}
; {:step "E", :in-degree 3}
; {:step "F", :in-degree 1}]

(defn get-in-degree-by-step
  "step 으로 in-degree value 가져오기
  (get-in-degree-by-step [{:step \"A\", :in-degree 1}
                           {:step \"B\", :in-degree 2}
                           {:step \"C\", :in-degree 0}
                           {:step \"D\", :in-degree 2}
                           {:step \"E\", :in-degree 3}
                           {:step \"F\", :in-degree 1}] \"A\") => 1"
  [in-degrees step]
  (some #(if (= step (% :step)) (% :in-degree)) in-degrees))

(defn generate-in-degrees
  "reduce에 사용하는 in-degress 생성 함수"
  [in-degrees edge]
  (let [prev (first edge)
        next (second edge)]
    (map (fn [in-degree]
           (if (and (= next (in-degree :step))
                    (>= (get-in-degree-by-step in-degrees prev)
                        (get-in-degree-by-step in-degrees next)))
             {:step next :in-degree (inc (get-in-degree-by-step in-degrees prev))}
             in-degree))
         in-degrees)))


(defn generate-step-order
  "step 실행 순서대로 저장하는 벡터 생성
  
  [orders edges]
  ([[] [[C A] [C F] [A B] [A D] [B E] [D E] [F E]]]
  [[C] ([A B] [A D] [B E] [D E] [F E])]
  [[C A] ([B E] [D E] [F E])] 
  [[C A B] ([D E] [F E])] 
  [[C A B D] ([F E])] 
  [(C A B D F E) ()])
  "
  [[orders edges]]
  (let [in-degrees-of-step (->> edges
                                flatten
                                distinct
                                (map (fn [step] {:step step :in-degree 0})))
        order (->> edges
                   (reduce generate-in-degrees in-degrees-of-step)
                   (sort-by (juxt :in-degree :step))
                   first
                   :step)]
    [(if (= 1 (count edges))
       (->> (apply merge orders edges)
            flatten)
       (conj orders order))
     (filter #(not= order (first %)) edges)]))



(comment
  (->> (take 26 (iterate generate-step-order [[] step-edges]))
       last
       first
       flatten
       str/join))