(ns operator-finder.core
  (:require [clojure.string :as str]))

(defn parse-line 
  "Parse a single line into [expected-result [numbers]]"
  [line]
  (let [[result nums] (str/split line #":")
        numbers (map #(Long/parseLong %) (str/split (str/trim nums) #"\s+"))]
    [(Long/parseLong result) numbers]))

(defn evaluate 
  "Evaluate a sequence of numbers using given operators"
  [numbers operators]
  (reduce
    (fn [result [n op]]
      ((case op \+ + \* *) result n))
    (first numbers)
    (map vector (rest numbers) operators)))

(defn generate-operators 
  "Generate all possible combinations of + and * for given length"
  [len]
  (if (zero? len)
    [[]]
    (for [op [\+ \*]
          rest-ops (generate-operators (dec len))]
      (cons op rest-ops))))

(defn find-solution 
  "Find if there exists a solution for given expected result and numbers"
  [expected numbers]
  (let [ops-needed (dec (count numbers))
        possible-ops (generate-operators ops-needed)]
    (some #(= expected (evaluate numbers %)) possible-ops)))

(defn process-line 
  "Process a single line and return [value valid?]"
  [line]
  (let [[expected numbers] (parse-line line)
        has-solution? (find-solution expected numbers)]
    [expected has-solution?]))

(defn solve 
  "Process input string and return sum of valid equations"
  [input]
  (let [results (->> (str/split-lines input)
                     (remove str/blank?)
                     (map process-line)
                     (filter second)  ; keep only valid equations
                     (map first))]    ; get their values
    {:valid-equations (count results)
     :sum (apply + results)}))

(defn -main [& args]
  (let [filename (if (= (first args) "run")
                  "7.input"
                  "7.sample")
        input (slurp filename)
        {:keys [valid-equations sum]} (solve input)]
    (println "Number of valid equations:" valid-equations)
    (println "Sum of valid equations:" sum)
    sum))

(case (first *command-line-args*)
  nil (-main)
  (-main (first *command-line-args*)))

(shutdown-agents)