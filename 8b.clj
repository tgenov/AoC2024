(ns antenna-analyzer.core
  (:require [clojure.string :as str]))

(defn parse-input
  "Convert input string to a map of coordinates to frequencies"
  [input]
  (let [lines (str/split-lines input)]
    (->> (for [y (range (count lines))
               x (range (count (first lines)))
               :let [char (get-in lines [y x])]
               :when (not= char \.)]
           [[x y] char]))))

(defn collinear?
  "Check if three points are collinear"
  [[x1 y1] [x2 y2] [x3 y3]]
  (let [dx1 (- x2 x1)
        dy1 (- y2 y1)
        dx2 (- x3 x1)
        dy2 (- y3 y1)]
    (= (* dy1 dx2) (* dy2 dx1))))

(defn find-grid-bounds
  "Find the dimensions of the input grid"
  [input]
  (let [lines (str/split-lines input)]
    [(count (first lines)) (count lines)]))

(defn has-collinear-antennas?
  "Check if a point is collinear with at least two antennas of the same frequency"
  [point freq-groups]
  (some (fn [[freq coords]]
          (some (fn [a1]
                  (some (fn [a2]
                          (and (not= a1 a2)
                               (collinear? a1 a2 point)))
                        coords))
                coords))
        freq-groups))

(defn count-antinodes
  "Count all antinodes for a given set of antenna positions"
  [antennas width height]
  (let [freq-groups (group-by second antennas)
        antenna-coords-by-freq (into {} (map (fn [[k v]] [k (map first v)]) freq-groups))]
    (count
     (distinct
      (for [x (range width)
          y (range height)
          :let [point [x y]]
          :when (has-collinear-antennas? point antenna-coords-by-freq)]
      point)))))

(defn solve [input]
  (let [[width height] (find-grid-bounds input)]
    (count-antinodes (parse-input input) width height)))

(defn -main [& args]
  (let [filename (if (= (first args) "run")
                  "8.input"
                  "8.sample")
        input (slurp filename)
        result (solve input)]
    (println "Solution:" result)
    result))

(case (first *command-line-args*)
  nil (-main)
  (-main (first *command-line-args*)))

(shutdown-agents)