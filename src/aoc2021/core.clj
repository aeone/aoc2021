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

(defn parse-int [x] (Integer/parseInt x))
(defn pp [& args] (pprint/pprint args))
(defn t> [x msg] (pprint/pprint [msg x]) x)
(defn t>> [msg x] (pprint/pprint [msg x]) x)

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

(defn p1b [args]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn p4 [args]
  (let [extract (str/split args #"\n\n")
        [numbers & boards] extract
        ;; _ (t>> "numbers1" numbers)
        ;; _ (t>> "numbers1type" (type numbers))
        numbers (map parse-int (str/split numbers #","))
        ;; _ (t>> "numbers2" numbers)
        board-convert (fn [b] (->> b
                                   (str/split-lines)
                                   (map str/trim)
                                   (map (fn [s] (->> s
                                                     (#(str/split % #"\s+"))
                                                      ;; (t>> "board row")
                                                     (map str/trim)
                                                     (map parse-int))))))
        boards (map board-convert boards)
        ;; _ (t>> "boards" boards)
        single-row-wins (fn [drawn row] (every? identity (map #(drawn %) row)))
        board-row-wins (fn [drawn board] (some identity (map (partial single-row-wins drawn) board)))
        board-wins (fn [drawn board] 
                    ;;  (pp "drawn" drawn "board" board "inv-board" (apply mapv vector board)
                    ;;      "board-row-wins" (board-row-wins drawn board) 
                    ;;      "board-col-wins" (board-row-wins drawn (apply mapv vector board)))
                     (or (board-row-wins drawn board) (board-row-wins drawn (apply mapv vector board))))
        get-unmarked (fn [drawn board] (->> board (flatten) (filter #(not (drawn %)))))
        ;; _ (t>> "board wins" (board-wins (set numbers) (first boards)))
        ]
    (loop [drawn (set [(first numbers)]) just-called (first numbers) remaining (rest numbers) boards boards]
      (let [winning-boards (filter (partial board-wins drawn) boards)
            ;; _ (pprint/pprint ["drawn" drawn "winning-boards" winning-boards])
            [next-num & next-rem] remaining]
        (if (>= (count winning-boards) 1)
          (* just-called (apply + (get-unmarked drawn (first winning-boards))))
          (recur (conj drawn next-num) next-num next-rem boards))))))

(comment
  (every? identity (map #(#{0 7 24 4 21 17 2 23 11 9 5 14} %) '(14 21 17 24 4)))
  )