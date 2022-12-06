#! /usr/bin/env boot

(defn get-windows [n step data]
  (->> data 
       drop-last
       (partition n step)))

(defn unique? [window]
  (->> window
       set
       count
       (= (count window))))

(defn get-uniques [windows]
  (keep-indexed
    (fn [idx window]
      (if (unique? window)
        [idx window]
        nil))
    windows))

(defn get-packet-index [window-size windows]
  (->> windows
       get-uniques
       first
       first
       (+ window-size)))

(defn process-signal [window-size data]
  (->> data
       (get-windows window-size 1)
       (get-packet-index window-size)))

(defn -main [& _]
  (let [data (-> "input.txt"
                 slurp)
        start-of-packet-index (process-signal 4 data)
        start-of-message-index (process-signal 14 data)]
    (println start-of-packet-index)
    (println start-of-message-index)))
