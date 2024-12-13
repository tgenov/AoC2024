(ns garden-regions
  (:require [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-input [input]
  (vec (for [line (str/split-lines input)]
    (vec (map str (seq line))))))

(defn get-neighbors [[row col] grid]
  (let [max-row (dec (count grid))
        max-col (dec (count (first grid)))]
    (->> [[-1 0] [1 0] [0 -1] [0 1]]
         (map (fn [[dr dc]] [(+ row dr) (+ col dc)]))
         (filter (fn [[r c]]
                  (and (<= 0 r max-row)
                       (<= 0 c max-col)))))))

(defn find-region [start grid visited]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start)
         region #{}
         visited visited]
    (if (empty? queue)
      [region visited]
      (let [pos (peek queue)
            [row col] pos
            plant-type (get-in grid pos)]
        (if (visited pos)
          (recur (pop queue) region visited)
          (let [neighbors (filter #(and (not (visited %))
                                      (= plant-type (get-in grid %)))
                                (get-neighbors pos grid))]
            (recur (into (pop queue) neighbors)
                  (conj region pos)
                  (conj visited pos))))))))

(defn find-all-regions [grid]
  (loop [row 0
         col 0
         visited #{}
         regions []]
    (cond
      (>= row (count grid))
      regions
      
      (>= col (count (first grid)))
      (recur (inc row) 0 visited regions)
      
      (visited [row col])
      (recur row (inc col) visited regions)
      
      :else
      (let [[region new-visited] (find-region [row col] grid visited)]
        (recur row
               (inc col)
               new-visited
               (if (empty? region)
                 regions
                 (conj regions region)))))))

(defn calculate-perimeter [region grid]
  (let [type (get-in grid (first region))
        max-row (dec (count grid))
        max-col (dec (count (first grid)))]
    (reduce + 
      (for [pos region
            [dr dc] [[-1 0] [1 0] [0 -1] [0 1]]
            :let [r (+ (first pos) dr)
                  c (+ (second pos) dc)]
            :when (or 
                   (< r 0) (> r max-row)
                   (< c 0) (> c max-col)
                   (and (<= 0 r max-row)
                        (<= 0 c max-col)
                        (not= type (get-in grid [r c]))))]
        1))))

(defn get-region-type [region grid]
  (get-in grid (first region)))

(defn solve [input]
  (let [grid (parse-input input)
        regions (find-all-regions grid)]
    (reduce + (for [region regions
                    :let [area (count region)
                          perimeter (calculate-perimeter region grid)]]
                (* area perimeter)))))

(defn -main [& args]
  (let [filename (if (= (first args) "run")
                  "12.input"
                  "12.sample")
        input (slurp filename)]
    (println "Total price:" (solve input))
    (shutdown-agents)))

(case (first *command-line-args*)
  nil (-main)
  (-main (first *command-line-args*)))