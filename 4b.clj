(ns diagonal-pattern-counter
  (:require [clojure.string :as str]))

(defn read-grid [filename]
  (vec (map vec (str/split-lines (slurp filename)))))

(defn get-diagonals [grid row col]
  (let [diagonal1 (str (get-in grid [(dec row) (dec col)])
                       (get-in grid [(inc row) (inc col)]))
        diagonal2 (str (get-in grid [(dec row) (inc col)])
                       (get-in grid [(inc row) (dec col)]))]
    [diagonal1 diagonal2]))

(defn valid-diagonal? [s]
  (or (= s "MS") (= s "SM")))

(defn count-valid-as [grid]
  (count
   (for [row (range 1 (dec (count grid)))
         col (range 1 (dec (count (first grid))))
         :when (= (get-in grid [row col]) \A)
         :let [[d1 d2] (get-diagonals grid row col)]
         :when (and (valid-diagonal? d1)
                    (valid-diagonal? d2))]
     true)))

(println
 (count-valid-as
  (read-grid "4.input")))
