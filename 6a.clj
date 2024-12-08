(ns guard-path-tracker.core
  (:require [clojure.string :as str])
  (:gen-class))

(def directions
  {:up    [-1 0]
   :right [0 1]
   :down  [1 0]
   :left  [0 -1]})

(def direction-chars
  {\^ :up
   \> :right
   \v :down
   \< :left})

(defn char->direction [c]
  (get direction-chars c))

(defn turn-right [direction]
  (case direction
    :up    :right
    :right :down
    :down  :left
    :left  :up))

(defn parse-grid [input]
  (let [lines (str/split-lines input)
        height (count lines)
        width (count (first lines))
        grid (into {}
                   (for [y (range height)
                         x (range width)
                         :let [char (get-in lines [y x])]
                         :when (not= char \.)]
                     [[y x] char]))
        guard-pos (first (for [y (range height)
                               x (range width)
                               :let [char (get-in lines [y x])]
                               :when (direction-chars char)]
                           [y x]))
        guard-char (get-in lines (vec guard-pos))
        guard-direction (char->direction guard-char)]
    (when (nil? guard-pos)
      (throw (Exception. "No guard found in grid!")))
    (when (nil? guard-direction)
      (throw (Exception. (str "Invalid guard direction character: " guard-char))))
    {:grid (into {}
                 (for [y (range height)
                       x (range width)
                       :let [char (get-in lines [y x])]
                       :when (= char \#)]
                   [[y x] char]))
     :width width
     :height height
     :guard {:pos guard-pos
             :direction guard-direction}}))

(defn in-bounds? [{:keys [width height]} [y x]]
  (and (>= x 0) (< x width)
       (>= y 0) (< y height)))

(defn move [[y x] [dy dx]]
  (when (or (nil? y) (nil? x) (nil? dy) (nil? dx))
    (throw (Exception. (str "Invalid move coordinates: y=" y " x=" x " dy=" dy " dx=" dx))))
  [(+ y dy) (+ x dx)])

(defn walk-path [{:keys [grid width height] :as state} start-pos start-direction]
  (when (nil? start-pos)
    (throw (Exception. "Start position is nil")))
  (when (nil? start-direction)
    (throw (Exception. "Start direction is nil")))
  (loop [pos start-pos
         direction start-direction
         visited #{start-pos}]
    (let [dir-vec (get directions direction)
          _ (when (nil? dir-vec)
              (throw (Exception. (str "Invalid direction: " direction))))
          next-pos (move pos dir-vec)]
      (cond
        (not (in-bounds? state next-pos))
        visited

        (contains? grid next-pos)
        (recur pos
               (turn-right direction)
               visited)

        :else
        (recur next-pos
               direction
               (conj visited next-pos))))))

(defn solve [input]
  (let [state (parse-grid input)
        guard (:guard state)
        visited (walk-path state (:pos guard) (:direction guard))]
    (println "\nNumber of unique spaces visited:" (count visited))))

(defn -main [& args]
  (let [filename (if (= (first args) "run")
                   "6.input"
                   "6.sample")
        input (slurp filename)]
    (solve input)))

(case (first *command-line-args*)
  nil (-main)
  (-main (first *command-line-args*)))
(shutdown-agents)
