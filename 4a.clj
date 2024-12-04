(ns xmas-finder
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def directions
  [[0 1]   ; right
   [1 0]   ; down
   [1 1]   ; diagonal down-right
   [-1 1]  ; diagonal up-right
   [0 -1]  ; left
   [-1 0]  ; up
   [-1 -1] ; diagonal up-left
   [1 -1]]) ; diagonal down-left

(defn in-bounds? [grid row col]
  (and (>= row 0)
       (>= col 0)
       (< row (count grid))
       (< col (count (first grid)))))

(defn get-word [grid [start-row start-col] [delta-row delta-col] word-length]
  (loop [row start-row
         col start-col
         chars []]
    (if (or (not (in-bounds? grid row col))
            (= (count chars) word-length))
      (str/join chars)
      (recur (+ row delta-row)
             (+ col delta-col)
             (conj chars (get-in grid [row col]))))))

(defn find-word [grid word]
  (let [rows (count grid)
        cols (count (first grid))
        word-length (count word)]
    (for [row (range rows)
          col (range cols)
          [delta-row delta-col] directions
          :let [found-word (get-word grid [row col] [delta-row delta-col] word-length)]
          :when (= found-word word)]
      [row col delta-row delta-col])))

(defn mark-found-positions [grid word positions]
  (let [word-length (count word)
        marker-grid (vec (repeat (count grid)
                                 (vec (repeat (count (first grid)) \.))))]
    (reduce (fn [acc [row col delta-row delta-col]]
              (reduce (fn [grid-acc i]
                        (let [curr-row (+ row (* i delta-row))
                              curr-col (+ col (* i delta-col))]
                          (assoc-in grid-acc [curr-row curr-col]
                                    (nth word i))))
                      acc
                      (range word-length)))
            marker-grid
            positions)))

(defn parse-grid [input]
  (->> input
       str/split-lines
       (mapv vec)))

(defn print-grid [grid]
  (doseq [row grid]
    (println (str/join row))))

(defn solve-xmas [input]
  (let [grid (parse-grid input)
        positions (find-word grid "XMAS")]
    {:count (count positions)
     :marked-grid (mark-found-positions grid "XMAS" positions)}))

;; Read from file and solve
(let [input (slurp "4.input")
      {:keys [count marked-grid]} (solve-xmas input)]
  (println "Found" count "instances of XMAS:"))
