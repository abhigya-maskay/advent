#!/usr/bin/env boot

(require '[clojure.string :as str])
(require '[clojure.pprint :refer [pprint]])

(defn command? [split-line]
  (->> split-line first (= "$")))

(defn cd? [split-line]
  (->> split-line second (= "cd")))

(defn partition-ls [split-line]
  (if (and (command? split-line) (cd? split-line))
    :cd
    :ls))

(defn get-cd-target [split-line]
  (nth split-line 2))

(defn get-new-path [path group]
  (let [new-path (reduce
                  (fn [acc split-line]
                    (let [cd-target (get-cd-target split-line)]
                      (if (= cd-target "..")
                        (drop-last 2 acc)
                        (concat acc [cd-target :children]))))
                  path
                  group)]
    (into [] new-path)))

(defn process-cd [acc group]
  (update-in acc [:path] get-new-path group))

(defn dir? [meta]
  (= meta "dir"))

(defn get-tree [tree group path]
  (reduce
   (fn [tree [meta file-name]]
     (if (dir? meta)
       (assoc-in tree (conj path file-name) {:children {} :type :dir})
       (assoc-in tree (conj path file-name) {:size (Integer/parseInt meta) :type :file})))
   tree
   group))

(defn process-ls [acc group]
  (let [current-path (:path acc)
        ls-output (drop 1 group)]
    (update-in acc [:tree] get-tree ls-output current-path)))

(defn process-group [acc group]
  (let [initial-command (first group)]
    (if (cd? initial-command)
      (process-cd acc group)
      (process-ls acc group))))

(defn get-file-tree [grouped-data]
  (let [initial-acc {:tree {} :path []}]
    (:tree (reduce
            process-group
            initial-acc
            grouped-data))))

(defn size-dirs [file-tree]
  (->> file-tree
       (map
        (fn [[file-name file-info]]
          (if (->> file-info :size nil? not)
            [file-name file-info]
            [file-name (let [new-children (->> file-info
                                               :children
                                               size-dirs)
                             size (->> new-children
                                       (map (fn [[_ file-info]]
                                              (:size file-info)))
                                       (reduce +))]
                         (assoc file-info :size size :children new-children))])))
       (into {})))

(defn filter-nodes [predicate? file-tree]
  (reduce
    (fn [acc [_ file-info]]
      (let [is-dir? (->> file-info :type (= :dir))
            filtered-children (if is-dir? 
                                (filter-nodes predicate? (:children file-info))
                                [])]
        (if (predicate? file-info)
          (concat acc [file-info] filtered-children)
          (concat acc filtered-children))))
    []
    file-tree))


(defn -main [& _]
  (let [data (as-> "input.txt" i
               (slurp i)
               (str/split i #"\n")
               (map #(str/split % #" ") i))
        file-tree (as-> data d
                    (partition-by partition-ls d)
                    (get-file-tree d)
                    (assoc-in d ["/" :type] :dir))
        sized-file-tree (->> file-tree
                             size-dirs)
        large-dirs (filter-nodes
                    (fn [x]
                      (and
                        (->> x :type (= :dir))
                        (->> x :size (> 100000))))
                    sized-file-tree)
        sum-sizes (->> large-dirs
                       (map :size)
                       (reduce +))
        total-used-space (->> "/" sized-file-tree :size)
        free-space (- 70000000 total-used-space)
        needed-space (- 30000000 free-space)
        large-enough-dirs (filter-nodes
                            (fn [x]
                              (and
                                (->> x :type (= :dir))
                                (->> x :size (< needed-space))))
                            sized-file-tree)
        smallest-delete (->> large-enough-dirs
                             (map :size)
                             sort
                             first)]
    (pprint sum-sizes)
    (pprint smallest-delete)))
