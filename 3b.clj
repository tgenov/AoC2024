(ns mul-calculator
  (:require [clojure.string :as str]))

(defn process-instructions [text]
  (loop [pos 0
         enabled? true  ; Start with mul enabled
         sum 0
         remaining text]
    (if (empty? remaining)
      sum
      (let [do-match (re-find #"^do\(\)" remaining)
            dont-match (re-find #"^don't\(\)" remaining)
            mul-match (re-find #"^mul\((\d+),(\d+)\)" remaining)]
        (cond
          do-match
          (recur (+ pos (count do-match))
                 true
                 sum
                 (subs remaining (count do-match)))

          dont-match
          (recur (+ pos (count dont-match))
                 false
                 sum
                 (subs remaining (count dont-match)))

          mul-match
          (let [[full x y] mul-match
                product (if enabled?
                          (* (Integer/parseInt x) (Integer/parseInt y))
                          0)]
            (recur (+ pos (count full))
                   enabled?
                   (+ sum product)
                   (subs remaining (count full))))
          :else
          (recur (inc pos)
                 enabled?
                 sum
                 (subs remaining 1)))))))

(def text (slurp "3.input"))
(println (process-instructions text))
