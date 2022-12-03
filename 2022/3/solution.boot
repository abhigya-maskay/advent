#! /usr/bin/env boot

(require '[clojure.string :as str])
(require '[clojure.set :as s])

(defn split-compartments [items]
  (let [number (count items)]
    (->> items
         (partition (/ number 2))
         (map set))))

(defn get-char-range [start end]
  (map
   char
   (range
    (int start)
    (inc (int end)))))

(def priority-map
  (let [lowercase (get-char-range \a \z)
        uppercase (get-char-range \A \Z)
        characters (concat lowercase uppercase)]
    ( ->> characters
     (map-indexed
      (fn [idx chr] [chr (inc idx)]))
     (into {}))))

(defn -main [& args]
  (let [data (-> "input.txt"
                 slurp
                 (str/split #"\n"))
        with-split-compartments (map split-compartments data)
        intersections (mapcat
                       (partial apply s/intersection)
                       with-split-compartments)
        priorities (map priority-map intersections)
        groups (->> data
                    (partition 3)
                    (map (partial map set)))
        badges (mapcat (partial apply s/intersection) groups)
        badge-priorities (map priority-map badges)]
    (println (reduce + priorities))
    (println (reduce + badge-priorities))))
