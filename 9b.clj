(ns disk-compaction.core
  (:require [clojure.string :as str]))

(defn parse-disk-map
  "Convert disk map string into sequence of [length type] pairs
   where type is :file or :space"
  [input]
  (->> input
       str/trim
       (map (comp parse-long str))
       (map-indexed (fn [i n] [n (if (even? i) :file :space)]))
       (into [])))

(defn expand-to-blocks
  "Convert length/type pairs into sequence of individual blocks"
  [pairs]
  (loop [pairs pairs
         file-id 0
         result []]
    (if (empty? pairs)
      result
      (let [[length type] (first pairs)
            blocks (repeat length (if (= type :file) file-id \.))
            next-id (if (= type :file) (inc file-id) file-id)]
        (recur (rest pairs)
               next-id
               (into result blocks))))))

(defn get-file-info
  "Get map of file IDs to their positions and lengths"
  [blocks]
  (reduce (fn [acc [idx val]]
            (if (number? val)
              (update acc val
                      (fn [entry]
                        (if entry
                          (update entry :length inc)
                          {:start idx :length 1})))
              acc))
          {}
          (map-indexed vector blocks)))

(defn find-free-space
  "Find leftmost free space that can fit given length"
  [blocks needed-length]
  (loop [start 0
         current-length 0
         idx 0]
    (cond
      (>= idx (count blocks)) nil
      (= (nth blocks idx) \.) (recur (if (zero? current-length) idx start)
                                     (inc current-length)
                                     (inc idx))
      (>= current-length needed-length) start
      :else (recur (inc idx)
                   0
                   (inc idx)))))

(defn move-whole-file
  "Move a file to leftmost available space if possible"
  [blocks file-id {:keys [start length]}]
  (let [target-pos (find-free-space blocks length)]
    (if (and target-pos (< target-pos start))
      (let [file-blocks (repeat length file-id)
            empty-blocks (repeat length \.)
            before (subvec blocks 0 target-pos)
            middle (subvec blocks (+ target-pos length) start)
            after (subvec blocks (+ start length))]
        (vec (concat before file-blocks middle empty-blocks after)))
      blocks)))

(defn compact-disk
  "Move files from highest ID to lowest"
  [blocks]
  (let [file-info (get-file-info blocks)
        file-ids (sort > (keys file-info))]
    (reduce (fn [acc file-id]
              (move-whole-file acc file-id (get file-info file-id)))
            blocks
            file-ids)))

(defn calculate-checksum
  "Calculate final checksum based on position * file-id"
  [blocks]
  (->> blocks
       (map-indexed (fn [i v] (when (number? v) (* i v))))
       (remove nil?)
       (reduce +)))

(defn solve
  "Complete solution function"
  [input]
  (->> input
       parse-disk-map
       expand-to-blocks
       compact-disk
       calculate-checksum))

(defn -main [& args]
  (let [filename (if (= (first args) "run")
                   "9.input"
                   "9.sample")
        input (slurp filename)
        result (solve input)]
    (println "Solution:" result)
    result))

(case (first *command-line-args*)
  nil (-main)
  (-main (first *command-line-args*)))

(shutdown-agents)
