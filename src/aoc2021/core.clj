(ns aoc2021.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Util
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-int [x] (Integer/parseInt x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn p1 [args]
  (->> args
       str/split-lines
       (map parse-int)
       (partition 2 1)
       (filter #(apply < %))
       (count)))

(comment
  (p1 "199
200
208
210
200
207
240
269
260
263")
  
  (p1 (slurp "src/aoc2021/p1-input.txt")))

(defn p1b [args]
  (->> args
       str/split-lines
       (map parse-int)
       (partition 3 1)
       (partition 2 1)
       (map (fn [ts] (map #(apply + %) ts)))
       (filter #(apply < %))
       (count)))
       

(comment
  (p1b "199
200
208
210
200
207
240
269
260
263")

  (p1b (slurp "src/aoc2021/p1-input.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn p2 [args]
  (loop [hori 0 depth 0 steps (str/split-lines args)]
    (if (empty? steps) (* hori depth)
        (let [step (first steps)
              [ins val] (str/split step #" ")
              val (Integer/parseInt val)]
          (case ins 
            "forward" (recur (+ hori val) depth (rest steps))
            "down"    (recur hori (+ depth val) (rest steps))
            "up"      (recur hori (- depth val) (rest steps)))))))

(comment 
  (p2 "forward 5
down 5
forward 8
up 3
down 8
forward 2")

  (p2 (slurp "src/aoc2021/p2-input.txt")))

(defn p2b [args]
  (loop [hori 0 
         depth 0 
         aim 0 
         steps (str/split-lines args)]
    (if (empty? steps) (* hori depth)
        (let [step (first steps)
              [ins val] (str/split step #" ")
              val (Integer/parseInt val)]
          (case ins
            "forward" (recur (+ hori val) (+ depth (* aim val)) aim (rest steps))
            "down"    (recur hori depth (+ aim val) (rest steps))
            "up"      (recur hori depth (- aim val) (rest steps)))))))

(comment
  (p2b "forward 5
down 5
forward 8
up 3
down 8
forward 2")

  (p2b (slurp "src/aoc2021/p2-input.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn p3 [args]
  (let [rows (str/split-lines args)
        transposed (apply mapv vector rows)
        common (map frequencies transposed)
        max (->> common (map #(apply max-key val %)) (map first))
        min (->> common (map #(apply min-key val %)) (map first))
        gamma (->> max (apply str) (#(Integer/parseInt % 2)))
        epsilon (->> min (apply str) (#(Integer/parseInt % 2)))]
    (* gamma epsilon)))

(comment
  (p3 "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

  (p3 (slurp "src/aoc2021/p3-input.txt")))

(defn min-key-or-zero [m] (if (and (> (count (vals m)) 1) (apply = (vals m))) \0 (first (apply min-key val m))))
(defn p3b [args]
  (let [rows (str/split-lines args)
        oxy (loop [inputs rows
                   position 0]
              (let [transposed (apply mapv vector inputs)
                    common (map frequencies transposed)
                    max (->> common (map #(apply max-key val %)) (map first))
                    max-pos (nth max position)
                    filtered (filter #(= (nth % position) max-pos) inputs)]
                (if (= 1 (count filtered))
                  (first filtered)
                  (recur filtered (+ position 1)))))
        oxy (->> oxy (apply str) (#(Integer/parseInt % 2)))
        cos (loop [inputs rows
                   position 0]
              (let [transposed (apply mapv vector inputs)
                    common (map frequencies transposed)
                    min (->> common (map min-key-or-zero))
                    min-pos (nth min position)
                    filtered (filter #(= (nth % position) min-pos) inputs)]
                (if (>= 1 (count filtered))
                  (first filtered)
                  (recur filtered (+ position 1)))))
        cos (->> cos (apply str) (#(Integer/parseInt % 2)))]
    (* oxy cos)))

(comment
  (apply max-key val {0 5 1 5})
  (p3b "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

  (p3b (slurp "src/aoc2021/p3-input.txt")))