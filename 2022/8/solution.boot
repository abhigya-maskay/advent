#!/usr/bin/env boot

(require '[clojure.string :as str])
(require '[clojure.pprint :refer [pprint]])

(defn get-visible-row [row]
  (let [starting-height (first row)]
    (->> row
         (drop 1)
         (reduce
          (fn [{height :height
                visible :visible}
               current-height]
            (let [current-visible? (> current-height height)
                  new-visible (conj visible current-visible?)
                  new-height (if current-visible?
                               current-height
                               height)]
              {:height new-height :visible new-visible}))
          {:height starting-height :visible [true]})
         :visible)))

(defn get-visible-grid [grid]
  (let [row-size (count (first grid))
        inner-grid (->> grid (drop 1) (drop-last 1))
        inner-visible (map get-visible-row inner-grid)
        edge-row (repeat row-size true)]
    (as-> inner-visible g
      (cons edge-row g)
      (concat g [edge-row]))))

(defn transpose-grid [grid]
  (apply mapv vector grid))

(defn reverse-grid [grid]
  (map reverse grid))

(defn count-visibles [grid]
  (let [visible-grid (get-visible-grid grid)
        visible-grid-2 (->> grid
                            transpose-grid
                            get-visible-grid
                            transpose-grid)
        visible-grid-3 (->> grid
                            reverse-grid
                            get-visible-grid
                            reverse-grid)
        visible-grid-4 (->> grid
                            transpose-grid
                            reverse-grid
                            get-visible-grid
                            reverse-grid
                            transpose-grid)
        total-visible-grid (map
                            (fn [& rows]
                              (apply
                               map
                               (fn [& cells]
                                 (reduce
                                  #(or %1 %2)
                                  cells))
                               rows))
                            visible-grid
                            visible-grid-2
                            visible-grid-3
                            visible-grid-4)]
    (->> total-visible-grid
         (map #(filter identity %))
         (map count)
         (reduce +))))

(defn process-row [idx row]
  (let [current-value (nth row idx)
        remaining-row (drop (inc idx) row)]
    (loop [acc 0
           remaining-row remaining-row]
      (if (empty? remaining-row)
        acc
        (if (>= (first remaining-row) current-value)
             (inc acc)
             (recur (inc acc) (drop 1 remaining-row)))))))

(defn get-scenic-score [idx idy grid]
  (let [transposed-grid (transpose-grid grid)
        row (nth grid idx)
        col (nth transposed-grid idy)
        reversed-row (reverse row)
        reversed-col (reverse col)
        row-score (process-row idy row)
        col-score (process-row idx col)
        reverse-idx (->> idy (- (count row)) dec)
        reversed-row-score (process-row
                            reverse-idx
                            reversed-row)
        reverse-idy (->> idx (- (count col)) dec)
        reversed-col-score (process-row
                            reverse-idy
                            reversed-col)]
    (* row-score reversed-row-score col-score reversed-col-score)))

(defn get-scenic-score-grid [grid]
  (map-indexed
   (fn [idx row]
     (map-indexed
      (fn [idy _]
        (get-scenic-score idx idy grid))
      row))
   grid))

(defn -main [& _]
  (let [data (-> "input.txt"
                 slurp
                 (str/split #"\n"))
        grid (map (fn [x]
                    (as-> x x
                      (str/split x #"")
                      (map #(Integer/parseInt %) x))) data)
        visibles (count-visibles grid)
        scenic-score-grid (get-scenic-score-grid grid)
        max-scenic-score (->> scenic-score-grid 
                              (map #(apply max %))
                              (apply max))]
    (pprint visibles)
    (pprint max-scenic-score)))
