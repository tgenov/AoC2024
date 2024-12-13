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

(defn distance
  "Calculate Euclidean distance between two points"
  [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (Math/pow (- x2 x1) 2)
                (Math/pow (- y2 y1) 2))))

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

(defn count-antinodes
  "Count all antinodes for a given set of antenna positions"
  [antennas width height]
  (let [freq-groups (group-by second antennas)
        coords-by-freq (into {} (map (fn [[k v]] [k (map first v)]) freq-groups))]
    (count
     (distinct
      (for [[freq coords] coords-by-freq
            p1 coords
            p2 coords
            :when (not= p1 p2)
            :let [candidates (for [x (range width)
                                 y (range height)
                                 :let [p3 [x y]]
                                 :when (and (collinear? p1 p2 p3)
                                          (let [d1 (distance p1 p3)
                                                d2 (distance p2 p3)]
                                            (or (< (Math/abs (- d1 (* 2 d2))) 0.0001)
                                                (< (Math/abs (- d2 (* 2 d1))) 0.0001))))]
                             p3)]
            antinode candidates]
        antinode)))))

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