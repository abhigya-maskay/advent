#! /usr/bin/env boot

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def code-map
  {"A" :rock
   "B" :paper
   "C" :scissors
   "X" :rock
   "Y" :paper
   "Z" :scissors})

(def score-map
  {:rock 1
  :paper 2
  :scissors 3})

(def loss-map
  {:rock :scissors
   :scissors :paper
   :paper :rock})

(def win-map
  (set/map-invert loss-map))

(def outcome-map
  {"X" :lose
   "Y" :draw
   "Z" :win})

(defn won? [x1 x2]
  (= (win-map x2) x1))


(defn get-round-score [x1 x2]
  (let [shape-score (score-map x2)
        outcome-score (cond
                        (= x2 x1) 3
                        (won? x2 x1) 6
                        :else 0)]
    (+ shape-score outcome-score)))

(defn process-rounds [xs]
  (->> xs
       (map code-map)))

(defn get-shape [x1 x2]
  (case x2
  :draw x1
  :win (win-map x1)
  :lose (loss-map x1)))

(defn process-rounds-2 [[x1-code x2-code]]
  (def x1 (code-map x1-code))
  (def x2 (outcome-map x2-code))
  [(get-shape x1 x2) x2])

(defn get-round-score-2 [x2 outcome]
  (let [shape-score (score-map x2)
        outcome-score (case outcome
                        :draw 3
                        :win 6
                        :lose 0)]
    (+ shape-score outcome-score)))


(defn -main [& args]
  (let [data (-> "input.txt"
                 slurp
                 (str/split #"\n"))
        lines (map #(str/split % #" ") data)
        rounds (map process-rounds lines)
        get-score (fn [get-round-score rounds]
                    (->> rounds
                        (map #(apply get-round-score %))
                        (reduce +)))
        score (get-score get-round-score rounds)
        rounds-2 (map process-rounds-2 lines)
        score-2 (get-score get-round-score-2 rounds-2)]
    (println score)
    (println score-2)))
