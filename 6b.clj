(ns guard-loop-finder.core
  (:require [clojure.string :as str]
            [clojure.core.reducers :as r])
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
                    :when (= char \#)]
                [[y x] char]))
        guard-pos (first (for [y (range height)
                             x (range width)
                             :let [char (get-in lines [y x])]
                             :when (direction-chars char)]
                         [y x]))
        guard-char (get-in lines (vec guard-pos))
        guard-direction (char->direction guard-char)]
    {:grid grid
     :width width
     :height height
     :guard {:pos guard-pos
             :direction guard-direction}}))

(defn in-bounds? [{:keys [width height]} [y x]]
  (and (>= x 0) (< x width)
       (>= y 0) (< y height)))

(defn move [[y x] [dy dx]]
  [(+ y dy) (+ x dx)])

(defn walk-path-until-loop [{:keys [grid width height] :as state} start-pos start-direction]
  (loop [pos start-pos
         direction start-direction
         visited #{}]
    (let [visit-key [pos direction]
          dir-vec (get directions direction)
          next-pos (move pos dir-vec)]
      (cond
        (contains? visited visit-key)
        true
        
        (not (in-bounds? state next-pos))
        false
        
        (contains? grid next-pos)
        (recur pos
               (turn-right direction)
               (conj visited visit-key))
        
        :else
        (recur next-pos
               direction
               (conj visited visit-key))))))

(defn generate-positions [{:keys [width height]}]
  (for [y (range height)
        x (range width)]
    [y x]))

(defn check-position [state pos]
  (let [{:keys [grid guard]} state]
    (when (and (not= pos (:pos guard))
               (not (contains? grid pos))
               (walk-path-until-loop 
                 (assoc state :grid (assoc grid pos \#))
                 (:pos guard) 
                 (:direction guard)))
      pos)))

(defn parallel-count-loop-positions [state]
  (let [chunk-size 64  ; Adjust this based on your input size
        positions (generate-positions state)
        chunks (partition-all chunk-size positions)]
    (->> chunks
         (pmap (fn [chunk]
                 (count (keep #(check-position state %) chunk))))
         (reduce +))))

(defn solve [input]
  (let [state (parse-grid input)]
    (parallel-count-loop-positions state)))

(defn -main [& args]
  (let [filename (if (= (first args) "run")
                  "6.input"
                  "6.sample")
        input (slurp filename)
        count (solve input)]
    (println "Total positions that cause loops:" count)
    count))

(case (first *command-line-args*)
  nil (-main)
  (-main (first *command-line-args*)))
(shutdown-agents)