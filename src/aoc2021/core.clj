(ns aoc2021.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]))

(defn -main
  "Run each puzzle solver via REPL!"
  [& args]
  (println "Hello, World!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Util
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-int ([x] (Integer/parseInt x))
                ([x y] (Integer/parseInt x y)))
(defn abs [x] (Math/abs x))
(defn pp [& args] (pprint/pprint args))
(defn t> [x msg] (pprint/pprint [msg x]) x)
(defn t>> [msg x] (pprint/pprint [msg x]) x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn day1 [args]
  (->> args
       str/split-lines
       (map parse-int)
       (partition 2 1)
       (filter #(apply < %))
       (count)))

(defn day1b [args]
  (->> args
       str/split-lines
       (map parse-int)
       (partition 3 1)
       (partition 2 1)
       (map (fn [ts] (map #(apply + %) ts)))
       (filter #(apply < %))
       (count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn day2 [args]
  (loop [hori 0 depth 0 steps (str/split-lines args)]
        (if (empty? steps) (* hori depth)
            (let [step      (first steps)
                  [ins val] (str/split step #" ")
                  val       (Integer/parseInt val)]
                 (case ins
                       "forward" (recur (+ hori val) depth (rest steps))
                       "down"    (recur hori (+ depth val) (rest steps))
                       "up"      (recur hori (- depth val) (rest steps)))))))

(defn day2b [args]
  (loop [hori 0 depth 0 aim 0 steps (str/split-lines args)]
        (if (empty? steps) (* hori depth)
            (let [step      (first steps)
                  [ins val] (str/split step #" ")
                  val       (Integer/parseInt val)]
                 (case ins
                       "forward" (recur (+ hori val) (+ depth (* aim val)) aim (rest steps))
                       "down"    (recur hori depth (+ aim val) (rest steps))
                       "up"      (recur hori depth (- aim val) (rest steps)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn day3 [args]
  (let [rows       (str/split-lines args)
        transposed (apply mapv vector rows)
        common     (map frequencies transposed)
        max        (->> common (map #(apply max-key val %)) (map first))
        min        (->> common (map #(apply min-key val %)) (map first))
        gamma      (->> max (apply str) (#(parse-int % 2)))
        epsilon    (->> min (apply str) (#(parse-int % 2)))]
       (* gamma epsilon)))

(defn min-key-or-zero [m] (if (and (> (count (vals m)) 1) (apply = (vals m)))
                              \0
                              (first (apply min-key val m))))
(defn day3b [args]
  (let [rows (str/split-lines args)
        find-last (fn [find-min-or-max] 
                      (loop [diag-nums rows
                             position 0]
                            (let [transposed (apply mapv vector diag-nums)
                                  count-of-ones-and-zeroes (-> transposed (nth position) frequencies)
                                  min-or-max (find-min-or-max count-of-ones-and-zeroes)
                                  filtered-diag-nums (filter #(= (nth % position) min-or-max) diag-nums)]
                                 (if (>= 1 (count filtered-diag-nums))
                                     (first filtered-diag-nums)
                                     (recur filtered-diag-nums (+ position 1))))))
        oxy (->> (fn [count-1s0s] (->> count-1s0s (#(apply max-key val %)) first))
                 find-last (apply str) (#(parse-int % 2)))
        cos (->> min-key-or-zero find-last (apply str) (#(parse-int % 2)))]
       (* oxy cos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn day4 [args]
  (let [extract (str/split args #"\n\n")
        [numbers & boards] extract
        numbers (map parse-int (str/split numbers #","))
        board-convert (fn [b] (->> b
                                   (str/split-lines)
                                   (map str/trim)
                                   (map (fn [s] (->> s
                                                     (#(str/split % #"\s+"))
                                                     (map parse-int))))))
        boards (map board-convert boards)
        single-row-wins (fn [drawn row] (every? identity (map #(drawn %) row)))
        board-row-wins (fn [drawn board] (some identity (map (partial single-row-wins drawn) board)))
        board-wins (fn [drawn board] (or (board-row-wins drawn board) 
                                         (board-row-wins drawn (apply mapv vector board))))
        get-unmarked (fn [drawn board] (->> board (flatten) (filter #(not (drawn %)))))]
       (loop [drawn (set [(first numbers)]) just-called (first numbers) remaining (rest numbers) boards boards]
             (let [winning-boards (filter (partial board-wins drawn) boards)
                   [next-num & next-rem] remaining]
                  (if (>= (count winning-boards) 1)
                      (* just-called (apply + (get-unmarked drawn (first winning-boards))))
                      (recur (conj drawn next-num) next-num next-rem boards))))))

(defn day4b [args]
  (let [extract (str/split args #"\n\n")
        [numbers & boards] extract
        numbers (map parse-int (str/split numbers #","))
        board-convert (fn [b] (->> b
                                   (str/split-lines)
                                   (map str/trim)
                                   (map (fn [s] (->> s
                                                     (#(str/split % #"\s+"))
                                                     (map parse-int))))))
        boards (map board-convert boards)
        single-row-wins (fn [drawn row] (every? identity (map #(drawn %) row)))
        board-row-wins (fn [drawn board] (some identity (map (partial single-row-wins drawn) board)))
        board-wins (fn [drawn board] (or (board-row-wins drawn board) 
                                         (board-row-wins drawn (apply mapv vector board))))
        get-unmarked (fn [drawn board] (->> board (flatten) (filter #(not (drawn %)))))]
       (loop [drawn (set [(first numbers)]) just-called (first numbers) remaining (rest numbers) boards boards]
             (let [losing-boards (filter #(not (board-wins drawn %)) boards)
                   [next-num & next-rem] remaining]
                  (if (= (count losing-boards) 0)
                      (* just-called (apply + (get-unmarked drawn (first boards))))
                      (recur (conj drawn next-num) next-num next-rem losing-boards))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn inclusive-range [x y] (range x (if (> y x) (inc y) (dec y)) (if (> y x) 1 -1)))
(defn coord-range [x1 y1 x2 y2]
                  (let [size (inc (max (abs (- x1 x2)) (abs (- y1 y2))))
                        xs (->> (inclusive-range x1 x2) cycle (take size))
                        ys (->> (inclusive-range y1 y2) cycle (take size))]
                       (for [[x y] (map vector xs ys)] [x y])))

(defn day5 [args]
  (let [extract (str/split-lines args)
        pairs (map #(->> % (re-find #"(\d+),(\d+) -> (\d+),(\d+)") (drop 1)) extract)
        pairs (map (fn [x] (map parse-int x)) pairs)
        points (for [[x1 y1 x2 y2] pairs
                     :when (or (= x1 x2) (= y1 y2))
                     [x y] (coord-range x1 y1 x2 y2)]
                    [x y])
        freqs (frequencies points)
        overlap (->> freqs vals (filter #(> % 1)) count)]
       overlap))

(defn day5b [args]
  (let [extract (str/split-lines args)
        pairs (map #(->> % (re-find #"(\d+),(\d+) -> (\d+),(\d+)") (drop 1)) extract)
        pairs (map (fn [x] (map parse-int x)) pairs)
        points (for [[x1 y1 x2 y2] pairs
                     [x y] (coord-range x1 y1 x2 y2)]
                    [x y])
        freqs (frequencies points)
        overlap (->> freqs vals (filter #(> % 1)) count)]
       overlap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn map-val-add [m k v] (+ (or (m k) 0) v))
(defn day6 [args]
  (let [fish (->> args (#(str/split % #",")) (map parse-int))]
       (loop [fish fish day 1]
             (let [new-fish (mapcat #(if (= % 0) [6 8] [(dec %)]) fish)]
                  (if (= day 80)
                      (->> new-fish count)
                      (recur new-fish (+ day 1)))))))

(defn day6b [args]
  (let [fish (->> args (#(str/split % #",")) (map parse-int) frequencies)]
    (loop [fish fish day 1]
      (let [new-fish (reduce (fn [acc [k v]] (if (= k 0)
                                                 (conj acc {6 (map-val-add acc 6 v)} {8 v})
                                                 (conj acc {(dec k) (map-val-add acc (dec k) v)}))) 
                             {} fish)]
        (if (= day 256)
          (->> new-fish vals (reduce +))
          (recur new-fish (+ day 1)))))))