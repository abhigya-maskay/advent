#!/usr/bin/env boot

(require '[clojure.string :as str])

(defn getInts [xs]
  (map #(Integer/parseInt %) xs))

(defn sum [ints]
  (reduce + ints))

(defn sumInts [xs]
  (-> xs getInts sum))

(defn -main [& args]
  (let [data (-> "input.txt"
             slurp
             (str/split #"\n\n"))
        calories (->> data
                     (map #(str/split % #"\n"))
                     (map sumInts))
        max-calories (apply max calories)
        top-three (->> calories
                       (sort >)
                       (take 3)
                       sum)]
    (println max-calories)
    (println top-three))
)
