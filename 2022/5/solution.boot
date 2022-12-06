#! /usr/bin/env boot

(require '[clojure.string :as str])
(require '[clojure.pprint :refer [pprint]])

(defn get-crate [cs]
  (->> cs
       (apply str)
       str/trim))

(defn get-crates [line]
  (map get-crate line))

(defn get-crate-positions [line]
  (map-indexed (fn [idx crate] [(inc idx) crate]) line))

(defn remove-blanks [crates-with-positions]
  (remove #(str/blank? (second %)) crates-with-positions))

(defn make-lists [crates-with-positions]
  (map (fn [[idx s]] [idx (list s)]) crates-with-positions))

(defn process-locations [lines]
  (->> lines
       (map
        (fn [line]
          (->> line
               (partition 4 4 [\space]))))
       (map get-crates)
       drop-last
       (map get-crate-positions)
       (map remove-blanks)
       (map make-lists)
       (map #(into {} %))
       (apply merge-with into)
       (map (fn [[idx l]]
              [idx (into [] l)]))
       (into {})))

(defn process-instructions [instruction]
  (->> instruction
       (drop 1)
       (take-nth 2)
       (map #(Integer/parseInt %))
       (map vector [:number :from :to])
       (into {})))

(defn place-moved-crates [acc to moved-crates]
  (concat (acc to) (reverse moved-crates)))

(defn place-moved-crates-2 [acc to moved-crates]
  (concat (acc to) moved-crates))

(defn execute-instruction [place-moved-crates acc instruction]
  (let [{number :number
         from :from
         to :to} instruction
        moved-crates (take-last number (acc from))
        new-from (drop-last number (acc from))
        new-to (place-moved-crates acc to moved-crates)]
    (assoc acc from new-from to new-to)))

(defn get-top-crates [crates]
  (->> crates
       (map (fn [[index crates]]
              [index (last crates)]))
       (sort-by (fn [[index _]]
                  index))
       (map
        (fn [[_ crate]]
          (->> crate
               second
               str)))
       str/join))

(defn -main [& _]
  (let [data (-> "input.txt"
                 slurp
                 (str/split #"\n\n"))
        crates (-> data
                   first
                   (str/split #"\n")
                   process-locations)
        instructions (as-> data d
                       (second d)
                       (str/split d #"\n")
                       (map #(str/split % #" ") d)
                       (map process-instructions d))
        final-crates (reduce
                      (fn [crates instruction]
                        (execute-instruction place-moved-crates crates instruction))
                      crates
                      instructions)
        top-crates (get-top-crates final-crates)
        final-crates-2 (reduce
                        (fn [crates instruction]
                          (execute-instruction place-moved-crates-2 crates instruction))
                        crates
                        instructions)
        top-crates-2 (get-top-crates final-crates-2)]
    (pprint top-crates)
    (pprint top-crates-2)))
