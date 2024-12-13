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

(defn find-rightmost-file
  "Find position of rightmost non-dot block"
  [blocks]
  (->> blocks
       (map-indexed vector)
       (filter (fn [[_ v]] (not= v \.)))
       last
       first))

(defn find-leftmost-space
  "Find position of leftmost dot"
  [blocks]
  (->> blocks
       (map-indexed vector)
       (filter (fn [[_ v]] (= v \.)))
       first
       first))

(defn move-file
  "Move one file block from right to left"
  [blocks]
  (let [right-pos (find-rightmost-file blocks)
        left-pos (find-leftmost-space blocks)
        moving-block (nth blocks right-pos)]
    (if (or (nil? right-pos) (nil? left-pos) (>= left-pos right-pos))
      blocks
      (-> blocks
          (assoc right-pos \.)
          (assoc left-pos moving-block)))))

(defn compact-disk
  "Repeatedly move files until fully compacted"
  [blocks]
  (->> blocks
       (iterate move-file)
       (partition 2 1)
       (take-while (fn [[a b]] (not= a b)))
       last
       second))

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
