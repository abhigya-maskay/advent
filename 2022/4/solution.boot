#! /usr/bin/env boot

(require '[clojure.string :as str])

(defn split-ranges [ranges]
  (map
   #(str/split % #"-")
   ranges))

(defn parse-ints [integers]
  (map #(Integer/parseInt %) integers))

(defn contains-range? [range1 range2]
  (let [[l1 r1] range1
        [l2 r2] range2]
    (and
     (<= l1 l2)
     (<= r2 r1))))

(defn is-subset? [range1 range2]
  (or
   (contains-range? range1 range2)
   (contains-range? range2 range1)))

(defn overlaps? [range1 range2]
  (let [[l1 r1] range1
        [l2 r2] range2]
    (and
     (<= l1 r2)
     (<= l2 r1))))

(defn count-matches [predicate xs]
  (->> xs (map (partial apply predicate))
       (filter identity)
       count))

(defn get-ranges [data]
  (->> data 
       (map #(str/split % #","))
       (map split-ranges)
       (map #(map parse-ints %))))

(defn -main [& _]
  (let [data (-> "input.txt"
                 slurp
                 (str/split #"\n"))
        ranges (get-ranges data)
        subsets (->> ranges
                     (count-matches is-subset?))
        overlaps (->> ranges
                      (count-matches overlaps?))]
    (println subsets)
    (println overlaps)))


