(ns list-processor.core
  (:require [clojure.string :as str]))

(defn process-file [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (let [; Read all lines from file
          lines (line-seq rdr)

          ; Parse each line into pairs of numbers
          pairs (map #(let [[a b] (str/split % #"\s+")]
                        [(Integer/parseInt a) (Integer/parseInt b)])
                     lines)
          ; Split pairs into two lists
          list1 (map first pairs)
          list2 (map second pairs)
          ; Sort both lists
          sorted1 (sort list1)
          sorted2 (sort list2)

          ; Calculate differences
          differences (map #(Math/abs (- %1 %2)) sorted1 sorted2)

          ; Calculate sum of differences
          total-difference (reduce + differences)]
      (println "Sum of differences:" total-difference))))

;; Run the program
(try
  (process-file "1.input")
  (catch java.io.FileNotFoundException e
    (println "Error: File '1.input' not found"))
  (catch Exception e
    (println "Error reading or processing file:" (.getMessage e))))
