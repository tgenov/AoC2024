(defn num-digits [n]
  (count (str n)))

(defn split-number [n]
  (let [s (str n)
        len (count s)
        mid (/ len 2)
        left (subs s 0 mid)
        right (subs s mid)]
    [(Integer/parseInt left) (Integer/parseInt right)]))

(defn transform-stone-freq [n freq]
  (cond
    (zero? n) {1 freq}
    (even? (num-digits n)) 
    (let [[left right] (split-number n)]
      (if (= left right)
        {left (* 2 freq)}  ; If both halves are the same, combine their frequencies
        {left freq, right freq}))
    :else {(* n 2024) freq}))

(defn transform-frequencies [stone-freqs]
  (reduce-kv 
    (fn [acc n freq]
      (let [new-freqs (transform-stone-freq n freq)]
        (merge-with + acc new-freqs)))
    {}
    stone-freqs))

(defn simulate-blinks [initial-stones n]
  (loop [freqs (frequencies initial-stones)
         blinks 0]
    (when (zero? (mod blinks 5))
      (println "Blink" blinks ":" (reduce + (vals freqs))))
    (if (= blinks n)
      (reduce + (vals freqs))
      (recur (transform-frequencies freqs) (inc blinks)))))

(defn parse-input [input]
  (->> (re-seq #"-?\d+" input)
       (mapv #(Integer/parseInt %))))

(defn solve [input]
  (let [initial-stones (parse-input input)]
    (simulate-blinks initial-stones 25)))

(defn -main [& args]
  (let [filename (if (= (first args) "run")
                  "11.input"
                  "11.sample")
        input (slurp filename)]
    (println "Final count:" (solve input))
    (shutdown-agents)))

(case (first *command-line-args*)
  nil (-main)
  (-main (first *command-line-args*)))