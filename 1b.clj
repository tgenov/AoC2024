(ns list-processor.core
  (:require [clojure.string :as str]))

(defn count-occurrences [item coll]
  (count (filter #(= item %) coll)))

(defn process-file [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (let [lines (line-seq rdr)
          pairs (map #(let [[a b] (str/split % #"\s+")]
                        [(Integer/parseInt a) (Integer/parseInt b)])
                     lines)
          list1 (map first pairs)
          list2 (map second pairs)
          unique-list1 (distinct list1)
          products (map #(* % (count-occurrences % list2)) unique-list1)
          total (reduce + products)]
      (println total))))

;; Run the program
(try
  (process-file "1.input")
  (catch java.io.FileNotFoundException e
    (println "Error: File '1.input' not found"))
  (catch Exception e
    (println "Error reading or processing file:" (.getMessage e))))
