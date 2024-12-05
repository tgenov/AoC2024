(ns advent.core
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

(defn valid-update? [update rule]
  (let [[before after] rule]
    (if (and (some #{before} update)
             (some #{after} update))
      (let [pos-before (.indexOf update before)
            pos-after (.indexOf update after)]
        (< pos-before pos-after))
      true)))

(defn validate-update [rules update]
  (every? #(valid-update? update %) rules))

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
        valid-updates (filter #(validate-update rules %) updates)
        middle-numbers (map get-middle valid-updates)
        sum (reduce + middle-numbers)]

    (println "\nSum of middle numbers:" sum)))

(case (first *command-line-args*)
  nil (-main)
  (-main (first *command-line-args*)))
(shutdown-agents)
