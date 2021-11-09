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
                           {:step \"C\", :in-degree 0}] \"A\") => 1"
  [in-degrees step]
  (some #(if (= step (% :step)) (% :in-degree)) in-degrees))

(defn generate-in-degrees
  "reduce에 사용하는 in-degress 생성 함수"
  [in-degrees [prev next]]
  (map (fn [in-degree]
         (if (and (= next (in-degree :step))
                  (>= (get-in-degree-by-step in-degrees prev)
                      (get-in-degree-by-step in-degrees next)))
           {:step next :in-degree (inc (get-in-degree-by-step in-degrees prev))}
           in-degree))
       in-degrees))


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
                                distinct ;node 추출
                                (map (fn [step] {:step step :in-degree 0}))) ;:in-degree 0으로 초기화
        order (->> edges
                   (reduce generate-in-degrees in-degrees-of-step)
                   (sort-by (juxt :in-degree :step)) ;첫번째 order 추출
                   first
                   :step)]
    [(if (= 1 (count edges))
       (->> (apply merge orders edges)
            flatten)
       (conj orders order))
     (filter #(not= order (first %)) edges)]))



(comment
  (->> (iterate generate-step-order [[] step-edges])
       (take 26)
       last
       first
       flatten
       str/join))

;===========[Part 2]===========

;node | parent
;A | C
;B | A
;C | -
;D | A
;E | B,D,F
;F | C

(defn parents-of-step 
  "먼저 끝내야 하는 부모 step을 찾아주는 함수
   (parents-of-step [[C A] [C F] [A B] [A D] [B E] [D E] [F E]] C) => nil
   (parents-of-step [[C A] [C F] [A B] [A D] [B E] [D E] [F E]] A) => C
   (parents-of-step [[C A] [C F] [A B] [A D] [B E] [D E] [F E]] E) => #{F B D}"
  [edges step]
  (->> edges
       (filter #(= step (second %)))
       (map first)
       set))

(parents-of-step step-edges "Q")

(parents-of-step [["C" "A"] ["C" "F"] ["A" "B"] ["A" "D"] ["B" "E"] ["D" "E"] ["F" "E"]] "C")
(parents-of-step [["C" "A"] ["C" "F"] ["A" "B"] ["A" "D"] ["B" "E"] ["D" "E"] ["F" "E"]] "E")


(defn second-of-step
  "소요 시간(second)를 계산
  (second-of-step \"A\") => 61"
  [step]
  (- (int (.charAt step 0)) 4))

(second-of-step "A")


(defn generate-order-for-workers
  "order-for-workers [C A F B D E]"
  [[order edges]]
  (let [steps (->> edges  ;중복 제거한 모든 step set
                   sort
                   flatten
                   distinct
                   set)
        root-steps (->> steps
                        (filter #(empty? (parents-of-step edges %)))
                        set)
        rest-steps (set/difference steps root-steps)
        rest-edges (filter #(not (root-steps (first %))) edges)
        order-with-root-steps (->> (conj order
                                         (sort (vec root-steps)))
                                   flatten
                                   vec)]

    [(if (seq rest-edges)
       order-with-root-steps
       (->> (conj order-with-root-steps
                  (sort (vec rest-steps)))
            flatten
            vec))
     rest-edges]))

(def step-edges-sample
  [["C" "A"] ["C" "F"] ["A" "B"] ["A" "D"] ["B" "E"] ["D" "E"] ["F" "E"]])


(def order-for-worker (->> (iterate generate-order-for-workers [[] step-edges])
                           (take 26)
                           last
                           first))


(defn get-end-of-parents [[edges worker-logs] step]
  (let [parents (parents-of-step edges step)
        max-end-parents (->> (filter #(parents (:step %)) worker-logs)
                             (sort-by (juxt :end :worker-id))
                             last
                             :end)
        fast-end (->> worker-logs
                      (sort-by (juxt :end :worker-id))
                      first
                      :end)]

    (if (seq parents)
      max-end-parents
      fast-end)))


(get-end-of-parents [step-edges-sample initial-worker-logs] "C")

(get-end-of-parents [step-edges initial-worker-logs] "X")


(defn index-of-worker-by-step [[edges worker-logs] step]
  (let [end-of-parents (get-end-of-parents [edges worker-logs] step)
        worker-logs-endpoint (->> (vals (group-by :worker-id worker-logs))
                                  (map #(sort-by :end %))
                                  (map last))
        worker-id (->> worker-logs-endpoint
                       (filter #(>= end-of-parents (:end %)))
                       (sort-by (juxt (fn [worker-log] (Math/abs (- (worker-log :end) end-of-parents))) :worker-id)) ;FIXME 남는 공간 없애기
                       first
                       :worker-id)
        end-fast-worker-id (->> worker-logs-endpoint
                                (sort-by (juxt :end :worker-id))
                                first
                                :worker-id)]

    (if (nil? worker-id)
      end-fast-worker-id
      worker-id)))


;(sort-by (juxt (fn [worker-log] (Math/abs (- (worker-log :end)
;                                             80)))
;               :worker-id)
;         [{:worker-id 2, :step "R", :start 0, :end 78}
;          {:worker-id 3, :step "T", :start 0, :end 80}])

;초기화
(def initial-worker-logs [{:worker-id 0 :step nil :start 0 :end 0}
                          {:worker-id 1 :step nil :start 0 :end 0}
                          {:worker-id 2 :step nil :start 0 :end 0}
                          {:worker-id 3 :step nil :start 0 :end 0}
                          {:worker-id 4 :step nil :start 0 :end 0}])

(index-of-worker-by-step [step-edges-sample [{:worker-id 0, :step nil, :start 0, :end 0}
                                             {:worker-id 1, :step nil, :start 0, :end 0}] ] "C")

(defn last-end
  "worker의 마지막으로 저장된 end"
  [worker-logs worker-id]
  (->> worker-logs
       (filter #(-> % :worker-id (= worker-id)))
       (apply max-key :end)
       :end))


(defn worker-log-by-step
  "worker-logs에 추가될 신규 log를 map으로 리턴"
  [[edges worker-logs] step]
  (let [end-of-parents (get-end-of-parents [edges worker-logs] step) ;parents-step의 end
        worker-id (index-of-worker-by-step [edges worker-logs] step)
        last-end-of-worker (last-end worker-logs worker-id) ;worker-id의 end
        start (apply max [end-of-parents last-end-of-worker])
        end (+ start (second-of-step step))]
    {:worker-id worker-id
     :step step
     :start start
     :end end}))


(defn generate-worker-logs [[edges worker-logs order-for-worker]]
  (let [order (first order-for-worker)]
    [edges
     (conj worker-logs (worker-log-by-step [edges worker-logs] order))
     (rest order-for-worker)]))

(comment
  (->> (iterate generate-worker-logs [step-edges initial-worker-logs order-for-worker])
       (take 27)
       last))

; worker-logs
; [{:worker-id 0 :step nil :start 0 :end 0}
;  {:worker-id 1 :step nil :start 0 :end 0}] [C A F B D E]

; [{:worker-id 0 :step nil :start 0 :end 0}
;  {:worker-id 1 :step nil :start 0 :end 0}
;  {:worker-id 0 :step "C" :start 0 :end 3}] [A F B D E]

; [{:worker-id 0 :step nil :start 0 :end 0}
;  {:worker-id 1 :step nil :start 0 :end 0}
;  {:worker-id 0 :step "C" :start 0 :end 3}
;  {:worker-id 0 :step "A" :start 3 :end 4}] [F B D E]

; [{:worker-id 0 :step nil :start 0 :end 0}
;  {:worker-id 1 :step nil :start 0 :end 0}
;  {:worker-id 0 :step "C" :start 0 :end 3}
;  {:worker-id 0 :step "A" :start 3 :end 4}
;  {:worker-id 1 :step "F" :start 3 :end 9}] [B D E]

; [{:worker-id 0 :step nil :start 0 :end 0}
;  {:worker-id 1 :step nil :start 0 :end 0}
;  {:worker-id 0 :step "C" :start 0 :end 3}
;  {:worker-id 0 :step "A" :start 3 :end 4}
;  {:worker-id 1 :step "F" :start 3 :end 9}
;  {:worker-id 0 :step "B" :start 4 :end 6} ] [D E]

; [{:worker-id 0 :step nil :start 0 :end 0}
;  {:worker-id 1 :step nil :start 0 :end 0}
;  {:worker-id 0 :step "C" :start 0 :end 3}
;  {:worker-id 0 :step "A" :start 3 :end 4}
;  {:worker-id 1 :step "F" :start 3 :end 9}
;  {:worker-id 0 :step "B" :start 4 :end 6}
;  {:worker-id 1 :step "F" :start 3 :end 9} ] [E]

; [{:worker-id 0 :step nil :start 0 :end 0}
;  {:worker-id 1 :step nil :start 0 :end 0}
;  {:worker-id 0 :step "C" :start 0 :end 3}
;  {:worker-id 0 :step "A" :start 3 :end 4}
;  {:worker-id 1 :step "F" :start 3 :end 9}
;  {:worker-id 0 :step "B" :start 4 :end 6}
;  {:worker-id 1 :step "F" :start 3 :end 9}
;  {:worker-id 0 :step "E" :start 10 :end 15} ] [E]