(ns hiking-trails
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-map [input]
  (let [lines (str/split-lines input)
        height (count lines)
        width (count (first lines))]
    {:grid (mapv (fn [line]
                   (mapv #(Character/digit % 10) (seq line)))
                 lines)
     :height height
     :width width}))

(defn valid-pos? [{:keys [height width]} [y x]]
  (and (>= y 0) (< y height)
       (>= x 0) (< x width)))

(defn get-height [{:keys [grid]} [y x]]
  (get-in grid [y x]))

(def directions [[0 1] [1 0] [0 -1] [-1 0]])  ; right, down, left, up

(defn neighbors [{:keys [grid] :as map-data} [y x]]
  (for [[dy dx] directions
        :let [new-y (+ y dy)
              new-x (+ x dx)]
        :when (valid-pos? map-data [new-y new-x])]
    [new-y new-x]))

(defn find-trailheads [{:keys [grid height width] :as map-data}]
  (for [y (range height)
        x (range width)
        :when (zero? (get-height map-data [y x]))]
    [y x]))

(defn find-paths
  ([map-data start]
   (find-paths map-data start #{} []))
  ([map-data pos visited path]
   (let [current-height (get-height map-data pos)]
     (if (= current-height 9)
       #{(conj path pos)}
       (let [valid-next (for [next-pos (neighbors map-data pos)
                             :when (and (not (visited next-pos))
                                      (= (get-height map-data next-pos)
                                         (inc current-height)))]
                         next-pos)]
         (apply set/union
                (for [next-pos valid-next]
                  (find-paths map-data
                             next-pos
                             (conj visited pos)
                             (conj path pos)))))))))

(defn count-reachable-nines [map-data start]
  (let [paths (find-paths map-data start)]
    (->> paths
         (map last)  ; Get end positions of all paths
         set
         count)))

(defn solve [input]
  (let [map-data (parse-map input)
        trailheads (find-trailheads map-data)
        scores (map #(count-reachable-nines map-data %) trailheads)]
    (reduce + scores)))

(defn -main [& args]
  (let [filename (if (= (first args) "run")
                  "10.input"
                  "10.sample")
        input-future (future (slurp filename))
        input @input-future
        result (solve input)]
    (println "Solution:" result)
    result))

(case (first *command-line-args*)
  nil (-main)
  (-main (first *command-line-args*)))

(shutdown-agents)