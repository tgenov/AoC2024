(require '[clojure.string :as str])

(defn validate-sequence [nums]
  (let [first-diff (- (second nums) (first nums))
        expected-direction? (if (pos? first-diff) pos? neg?)]
    (if (> (abs first-diff) 3)
      :unsafe
      (loop [curr (rest nums)
             prev (first nums)]
        (if (empty? curr)
          :safe
          (let [diff (- (first curr) prev)]
            (if (or (not (expected-direction? diff))
                    (< (abs diff) 1)
                    (>= (abs diff) 4))
              :unsafe
              (recur (rest curr) (first curr)))))))))

(defn parse-line [line]
  (->> (str/split line #"\s+")
       (map #(Integer/parseInt %))))

(let [safe-count (->> (slurp "2.input")
                     str/split-lines
                     (map parse-line)
                     (map validate-sequence)
                     (filter #(= % :safe))
                     count)]
  (println "Number of safe sequences:" safe-count))