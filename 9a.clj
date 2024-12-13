(ns disk-compaction.core
  (:require [clojure.string :as str])
  (:import (java.util ArrayList)))

;; Use transient collections for better performance in tight loops
(defn parse-disk-map
  "Convert disk map string into sequence of [length type] pairs
   where type is :file or :space"
  [input]
  (persistent!
   (reduce-kv
    (fn [acc i c]
      (conj! acc [(Character/digit c 10) (if (even? i) :file :space)]))
    (transient [])
    (vec (str/trim input)))))

;; Use Java's ArrayList for better performance in expand-to-blocks
(defn expand-to-blocks
  "Convert length/type pairs into sequence of individual blocks"
  [pairs]
  (let [result (ArrayList.)]
    (loop [pairs pairs
           file-id 0]
      (if (empty? pairs)
        (vec result)  ; Convert back to Clojure vector at the end
        (let [[length type] (first pairs)
              value (if (= type :file) file-id \.)
              next-id (if (= type :file) (inc file-id) file-id)]
          (dotimes [_ length]
            (.add result value))
          (recur (rest pairs) next-id))))))

;; Optimize finding positions by using loop/recur
(defn find-rightmost-file
  "Find position of rightmost non-dot block"
  [^ArrayList blocks]
  (loop [idx (dec (count blocks))]
    (when (>= idx 0)
      (if (not= (nth blocks idx) \.)
        idx
        (recur (dec idx))))))

(defn find-leftmost-space
  "Find position of leftmost dot"
  [^ArrayList blocks]
  (loop [idx 0
         len (count blocks)]
    (when (< idx len)
      (if (= (nth blocks idx) \.)
        idx
        (recur (inc idx) len)))))

;; Use mutable ArrayList for move operations
(defn move-file
  "Move one file block from right to left"
  [^ArrayList blocks]
  (let [blocks-copy (ArrayList. blocks)
        right-pos (find-rightmost-file blocks-copy)
        left-pos (find-leftmost-space blocks-copy)]
    (if (or (nil? right-pos) (nil? left-pos) (>= left-pos right-pos))
      blocks-copy
      (let [moving-block (.get blocks-copy right-pos)]
        (.set blocks-copy right-pos \.)
        (.set blocks-copy left-pos moving-block)
        blocks-copy))))

;; Use reduced to short-circuit when no more moves are possible
(defn compact-disk
  "Repeatedly move files until fully compacted"
  [blocks]
  (let [initial (ArrayList. blocks)]
    (reduce (fn [prev _]
              (let [next (move-file prev)]
                (if (= (seq prev) (seq next))
                  (reduced next)
                  next)))
            initial
            (range (count blocks)))))

;; Optimize checksum calculation with primitive operations
(defn calculate-checksum
  "Calculate final checksum based on position * file-id"
  [blocks]
  (loop [idx 0
         sum 0]
    (if (>= idx (count blocks))
      sum
      (let [v (nth blocks idx)]
        (recur (inc idx)
               (if (number? v)
                 (+ sum (* idx v))
                 sum))))))

(defn solve
  "Complete solution function"
  [input]
  (->> input
       parse-disk-map
       expand-to-blocks
       compact-disk
       calculate-checksum))

;; Use future for parallel file reading
(defn -main [& args]
  (let [filename (if (= (first args) "run")
                   "9.input"
                   "9.sample")
        input-future (future (slurp filename))
        input @input-future
        result (solve input)]
    (println "Solution:" result)
    result))

(case (first *command-line-args*)
  nil (-main)
  (-main (first *command-line-args*)))

(shutdown-agents)
