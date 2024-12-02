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

(defn try-dampener [nums]
  (if (= :safe (validate-sequence nums))
    true
    ;; Try removing each number one at a time
    (let [len (count nums)]
      (loop [i 0]
        (if (= i len)
          false
          (let [dampened (concat (take i nums) (drop (inc i) nums))]
            (if (= :safe (validate-sequence dampened))
              true
              (recur (inc i)))))))))

(defn parse-line [line]
  (->> (str/split line #"\s+")
       (map #(Integer/parseInt %))))

(let [safe-count (->> (slurp "2.input")
                     str/split-lines
                     (map parse-line)
                     (filter try-dampener)
                     count)]
  (println "Number of safe sequences with Problem Dampener:" safe-count))