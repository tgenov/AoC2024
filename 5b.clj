(ns day5
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (let [[rules-str updates-str] (clojure.string/split input #"\n\n")
        rules (map #(let [[a b] (clojure.string/split % #"\|")]
                      [(Integer/parseInt a) (Integer/parseInt b)])
                   (clojure.string/split-lines rules-str))
        updates (map #(map (fn [x] (Integer/parseInt x))
                           (clojure.string/split % #","))
                     (clojure.string/split-lines updates-str))]
    {:rules rules
     :updates updates}))

(defn validate-update [rules update]
  (every? (fn [[before after]]
            (if (and (some #{before} update)
                     (some #{after} update))
              (< (.indexOf update before) (.indexOf update after))
              true))
          rules))

; Count the number of times a number apepars on the Left-side of a rule 
; The higher the rank - the further left in the update.
(defn get-rank [rules n]
  (count (filter #(= n (first %)) rules)))

; Use the rules as tie-break for equal rankings
(defn must-come-before? [rules a b]
  (some #(and (= (first %) a) (= (second %) b)) rules))

(defn compare-numbers [rules a b]
  (let [rank-a (get-rank rules a)
        rank-b (get-rank rules b)]
    (if (= rank-a rank-b)
      (if (must-come-before? rules a b)
        -1
        (if (must-come-before? rules b a)
          1
          0))  ; if no rule specifies order, maintain original order
      (* -1 (compare rank-a rank-b)))))

(defn reorder-update [rules update]
  (vec (sort #(compare-numbers rules %1 %2) update)))

(defn get-middle [update]
  (nth update (quot (count update) 2)))

(defn -main [& args]
  (let [filename (if (= (first args) "run")
                   "5.input"
                   "5.sample")
        input (slurp filename)
        parsed (parse-input input)
        rules (:rules parsed)
        updates (:updates parsed)
        invalid-updates (filter #(not (validate-update rules %)) updates)
        fixed-updates (map #(reorder-update rules %) invalid-updates)
        middle-numbers (map get-middle fixed-updates)
        sum (reduce + middle-numbers)]

    (println "Sum of middle numbers:" sum)))

(case (first *command-line-args*)
  nil (-main)
  (-main (first *command-line-args*)))
(shutdown-agents)
