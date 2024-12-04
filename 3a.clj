(ns mul-calculator
  (:require [clojure.string :as str]))

(def text (slurp "3.input"))
(defn process-muls [text]
  (->> text
       (re-seq #"mul\((\d+),(\d+)\)")
       (map (fn [[_ x y]]
              (let [num1 (Integer/parseInt x)
                    num2 (Integer/parseInt y)]
                (* num1 num2))))
       (reduce +)))

(let [result (process-muls text)]
  (println result))
