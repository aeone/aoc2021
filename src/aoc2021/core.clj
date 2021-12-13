(ns aoc2021.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clojure.set :as set]))

(defn -main
  "Run each puzzle solver via REPL!"
  [& args]
  (println "Hello, World!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Util
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-int ([x] (Integer/parseInt x))
  ([x y] (Integer/parseInt x y)))
(defn char-parse-int [x] (Character/digit x 10))
(defn two-dimensional-map [f coll] (map #(map f %) coll))
(defn two-dimensional-mapv [f coll] (mapv #(mapv f %) coll))
(defn abs [x] (Math/abs x))
(defn uuid [] (.toString (java.util.UUID/randomUUID)))
(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))
(def not-in? (complement in?))
(defn pp [& args] (pprint/pprint args))
(defn t> [x msg] (pprint/pprint [msg x]) x)
(defn t>> [msg x] (pprint/pprint [msg x]) x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn day4 [args]
  (let [extract            (str/split args #"\n\n")
        [numbers & boards] extract
        numbers            (map parse-int (str/split numbers #","))
        board-convert      (fn [b] (->> b
                                        (str/split-lines)
                                        (map str/trim)
                                        (map (fn [s] (->> s
                                                          (#(str/split % #"\s+"))
                                                          (map parse-int))))))
        boards             (map board-convert boards)
        single-row-wins    (fn [drawn row]   (every? identity (map #(drawn %) row)))
        board-row-wins     (fn [drawn board] (some identity (map (partial single-row-wins drawn) board)))
        board-wins         (fn [drawn board] (or (board-row-wins drawn board)
                                                 (board-row-wins drawn (apply mapv vector board))))
        get-unmarked       (fn [drawn board] (->> board (flatten) (filter #(not (drawn %)))))]
       (loop [drawn (set [(first numbers)]) 
              just-called (first numbers) 
              remaining (rest numbers) 
              boards boards]
             (let [winning-boards (filter (partial board-wins drawn) boards)
                   [next-num & next-rem] remaining]
                  (if (>= (count winning-boards) 1)
                      (* just-called (apply + (get-unmarked drawn (first winning-boards))))
                      (recur (conj drawn next-num) next-num next-rem boards))))))

(defn day4b [args]
  (let [extract            (str/split args #"\n\n")
        [numbers & boards] extract
        numbers            (map parse-int (str/split numbers #","))
        board-convert      (fn [b] (->> b
                                        (str/split-lines)
                                        (map str/trim)
                                        (map (fn [s] (->> s
                                                          (#(str/split % #"\s+"))
                                                          (map parse-int))))))
        boards             (map board-convert boards)
        single-row-wins    (fn [drawn row] (every? identity (map #(drawn %) row)))
        board-row-wins     (fn [drawn board] (some identity (map (partial single-row-wins drawn) board)))
        board-wins         (fn [drawn board] (or (board-row-wins drawn board)
                                                 (board-row-wins drawn (apply mapv vector board))))
        get-unmarked       (fn [drawn board] (->> board flatten (filter #(not (drawn %)))))]
       (loop [drawn (set [(first numbers)]) 
              just-called (first numbers) 
              remaining (rest numbers) 
              boards boards]
             (let [losing-boards (filter #(not (board-wins drawn %)) boards)
                   [next-num & next-rem] remaining]
                  (if (= (count losing-boards) 0)
                      (* just-called (apply + (get-unmarked drawn (first boards))))
                      (recur (conj drawn next-num) next-num next-rem losing-boards))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn inclusive-range ([x] (inclusive-range 0 x))
                      ([x y] (range x (if (> y x) (inc y) (dec y)) (if (> y x) 1 -1))))
(defn coord-range [x1 y1 x2 y2]
  (let [size (inc (max (abs (- x1 x2)) (abs (- y1 y2))))
        xs   (->> (inclusive-range x1 x2) cycle (take size))
        ys   (->> (inclusive-range y1 y2) cycle (take size))]
       (for [[x y] (map vector xs ys)] [x y])))

(defn day5 [args]
  (let [extract (str/split-lines args)
        pairs   (map #(->> % (re-find #"(\d+),(\d+) -> (\d+),(\d+)") (drop 1)) extract)
        pairs   (map (fn [x] (map parse-int x)) pairs)
        points  (for [[x1 y1 x2 y2] pairs
                      :when (or (= x1 x2) (= y1 y2))
                      [x y] (coord-range x1 y1 x2 y2)]
                     [x y])
        freqs   (frequencies points)
        overlap (->> freqs vals (filter #(> % 1)) count)]
       overlap))

(defn day5b [args]
  (let [extract (str/split-lines args)
        pairs   (map #(->> % (re-find #"(\d+),(\d+) -> (\d+),(\d+)") (drop 1)) extract)
        pairs   (map (fn [x] (map parse-int x)) pairs)
        points  (for [[x1 y1 x2 y2] pairs
                      [x y] (coord-range x1 y1 x2 y2)]
                     [x y])
        freqs   (frequencies points)
        overlap (->> freqs vals (filter #(> % 1)) count)]
       overlap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn triangle [n] (/ (* n (inc n)) 2))
(defn day7 [args]
  (let [positions (->> args (#(str/split % #",")) (map parse-int))
        min-pos (apply min positions)
        max-pos (apply max positions)
        fuel-spend (map (fn [candidate-pos] (->> positions (map #(abs (- % candidate-pos))) (reduce +)))
                        (range min-pos (inc max-pos)))]
       (apply min fuel-spend)))

(defn day7b [args]
  (let [positions (->> args (#(str/split % #",")) (map parse-int))
        min-pos (apply min positions)
        max-pos (apply max positions)
        fuel-spend (map (fn [candidate-pos] (->> positions
                                                 (map #(triangle (abs (- % candidate-pos))))
                                                 (reduce +)))
                        (range min-pos (inc max-pos)))]
       (apply min fuel-spend)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn day8 [args]
  (let [parse-so-line (fn [l] (->> l
                                   (#(str/split % #" \| "))
                                   (map #(str/split % #" "))
                                   ((fn [[_ output-vals]] output-vals))))
        output-vals-list (->> args (str/split-lines) (map parse-so-line))]
       (->> output-vals-list (map seq) flatten (map count) (filter #{2 3 4 7}) count)))

(defn day8b [args]
  (let [parse-so-line (fn [l] (->> l
                                   (#(str/split % #" \| "))
                                   (map #(str/split % #" "))
                                   ((fn [[signals output-vals]] [(map set signals) output-vals]))))
        sigs-and-outs (->> args (str/split-lines) (map parse-so-line))
        decipher (fn [[signals output-vals]]
                   (let [by-size (group-by count signals)
                         [seg-2 seg-3 seg-4 seg-7] (->> [2 3 4 7] (map by-size) (map first))
                         [segs-5 segs-6] (->> [5 6] (map by-size))
                         a (set/difference seg-3 seg-2)
                         d (apply set/intersection seg-4 segs-5)
                         f (apply set/intersection seg-2 segs-6)
                         c (set/difference seg-2 f)
                         b (set/difference seg-4 c d f)
                         g (set/difference (apply set/intersection segs-6) a b f)
                         e (set/difference seg-7 a b c d f g)
                         segments {(set/union a b c e f g)   0
                                   (set/union c f)           1
                                   (set/union a c d e g)     2
                                   (set/union a c d f g)     3
                                   (set/union b c d f)       4
                                   (set/union a b d f g)     5
                                   (set/union a b d e f g)   6
                                   (set/union a c f)         7
                                   (set/union a b c d e f g) 8
                                   (set/union a b c d f g)   9}
                         dec-output (fn [o] (-> o set segments))]
                     (->> output-vals (map dec-output) (apply str) parse-int)))]
       (->> sigs-and-outs (map decipher) (reduce +))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 9
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn day9 [args]
  (let [lines (->> args str/split-lines (two-dimensional-mapv char-parse-int))
        x-size (count (first lines))
        y-size (count lines)
        adjacent (reduce (fn [acc [x y]] (let [n (get-in lines [y x])
                                               adj [(get-in lines [y (dec x)])
                                                    (get-in lines [y (inc x)])
                                                    (get-in lines [(inc y) x])
                                                    (get-in lines [(dec y) x])]
                                               adj (filter identity adj)]
                                           (conj acc [n adj])))
                         []
                         (for [x (range x-size) y (range y-size)] [x y]))
        lowest (filter (fn [[n adj]] (every? (partial < n) adj)) adjacent)]
       (reduce (fn [acc [n _]] (+ acc (inc n))) 0 lowest)))

(defn day9b [args]
  (let [lines (->> args str/split-lines (two-dimensional-mapv char-parse-int))
        x-size (count (first lines))
        y-size (count lines)
        basins (reduce (fn [acc [x y]]
                         (let [n (get-in lines [y x])
                               adj [[y (dec x)]
                                    [y (inc x)]
                                    [(inc y) x]
                                    [(dec y) x]]
                               adj (->> adj
                                        (map #(vector % (get-in lines %)))
                                        (filter second)
                                        (filter #(not= 9 (second %)))
                                        (map first))
                               unrelated-basins (filter #(not-any? % adj) acc)
                               related-basins (filter #(some % adj) acc)]
                              (cond (= 9 n) acc
                                    (empty? related-basins) (conj acc #{[y x]})
                                    :else (conj unrelated-basins (apply set/union #{[y x]} related-basins)))))
                       []
                       (for [x (range x-size) y (range y-size)] [x y]))]
       (->> basins (map count) sort reverse (take 3) (reduce *))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn day10 [args]
  (let [lines            (str/split-lines args)
        char-score       {\) 3 \] 57 \} 1197 \> 25137}
        open-brackets    (set "([{<")
        close-brackets   {\) \( \] \[ \} \{ \> \<}
        get-illegal-char (fn get-illegal-char
                             ([line] (get-illegal-char [] (-> line vec)))
                             ([stack input]
                              (let [[char & rest-of-chars] input
                                    valid-terminating-line (and (nil? char) (empty? stack))
                                    char-opens-bracket     (open-brackets char)
                                    char-closes-bracket    (and (close-brackets char)
                                                                (= (close-brackets char) (peek stack)))]
                                   (cond valid-terminating-line nil
                                         char-opens-bracket     (recur (conj stack char) rest-of-chars)
                                         char-closes-bracket    (recur (pop stack) rest-of-chars)
                                         :else                  (char-score char)))))]
       (->> lines (map get-illegal-char) (filter identity) (reduce +))))

(defn day10b [args]
  (let [lines          (str/split-lines args)
        char-score     {\( 1 \[ 2 \{ 3 \< 4}
        open-brackets  (set "([{<")
        close-brackets {\) \( \] \[ \} \{ \> \<}
        calc-score     (fn [rem-chars] (reduce (fn [acc n] (+ (* 5 acc) (char-score n))) 0 rem-chars))
        get-line-score (fn score
                           ([line] (score [] (-> line vec)))
                           ([stack input]
                            (let [[char & rest-of-chars] input
                                  valid-terminating-line (and (nil? char) (empty? stack))
                                  incomplete-line        (and (nil? char) (seq stack))
                                  char-opens-bracket     (open-brackets char)
                                  char-closes-bracket    (and (close-brackets char)
                                                              (= (close-brackets char) (peek stack)))]
                                 (cond valid-terminating-line nil
                                       incomplete-line        (calc-score (reverse stack))
                                       char-opens-bracket     (recur (conj stack char) rest-of-chars)
                                       char-closes-bracket    (recur (pop stack) rest-of-chars)
                                       :else                  nil))))]
       (->> lines (map get-line-score) (filter identity) sort (#(nth % (quot (count %) 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn safe-update-in [m ks f] (if (get-in m ks) (update-in m ks f) m))
(defn day11 [args]
  (let [grid (->> args (str/split-lines) (two-dimensional-mapv char-parse-int))
        y-size (count grid)
        x-size (count (first grid))
        inc-unless-f #(if (#{"F"} %) "F" (inc %))
        zero-if-f #(if (#{"F"} %) 0 %)
        flash (fn [grid y x]
                  (let [adj-coords (for [x (inclusive-range (dec x) (inc x)) y (inclusive-range (dec y) (inc y))] [y x])]
                       (-> (reduce (fn [grid coord] (safe-update-in grid coord inc-unless-f))
                                   grid adj-coords)
                           (assoc-in [y x] "F"))))
        step (fn [grid]
                 (let [coords          (for [x (range x-size) y (range y-size)] [y x])
                       coord-flashing? #(let [n (get-in grid %)] (and (number? n) (> n 9)))
                       flashing-coords (filter coord-flashing? coords)]
                      (if (empty? flashing-coords)
                          grid
                          (recur (apply flash grid (first flashing-coords))))))
        run-steps (fn [grid steps flashes]
                      (if (zero? steps) flashes
                          (recur (->> grid 
                                      (two-dimensional-mapv zero-if-f) 
                                      (two-dimensional-mapv inc) 
                                      step)
                                 (dec steps)
                                 (+ flashes (count (filter #{"F"} (flatten grid)))))))]
    (run-steps grid (inc 100) 0)))

(defn day11b [args]
  (let [grid (->> args (str/split-lines) (two-dimensional-mapv char-parse-int))
        y-size (count grid)
        x-size (count (first grid))
        inc-unless-f #(if (#{"F"} %) "F" (inc %))
        zero-if-f #(if (#{"F"} %) 0 %)
        flash (fn [grid y x]
                  (let [adj-coords (for [x (inclusive-range (dec x) (inc x)) y (inclusive-range (dec y) (inc y))] [y x])]
                       (-> (reduce (fn [grid coord] (safe-update-in grid coord inc-unless-f))
                                   grid adj-coords)
                           (assoc-in [y x] "F"))))
        step (fn [grid]
                 (let [coords          (for [x (range x-size) y (range y-size)] [y x])
                       coord-flashing? #(let [n (get-in grid %)] (and (number? n) (> n 9)))
                       flashing-coords (filter coord-flashing? coords)]
                      (if (empty? flashing-coords)
                          grid
                          (recur (apply flash grid (first flashing-coords))))))
        run-steps (fn [grid steps]
                      (if (every? #{"F"} (flatten grid)) steps
                          (recur (->> grid 
                                      (two-dimensional-mapv zero-if-f) 
                                      (two-dimensional-mapv inc) 
                                      step)
                                 (inc steps))))]
       (run-steps grid 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 12
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn day12 [args]
  (let [connections (->> args 
                         (str/split-lines) 
                         (map #(str/split % #"-")) 
                         (mapcat (fn [[a b]] [[a b] [b a]])) 
                         (group-by first) 
                         (map (fn [[a b]] [a (map second b)])))
        get-next (fn [node] (->> connections (filter #(= (first %) node)) first second))
        upper-case? #(= % (str/upper-case %))
        valid-next (fn [visited next] (or (upper-case? next) 
                                          (not-in? visited next)))
        navigate (fn [current-paths routes]
                     (let [current-path (first current-paths)
                           current-node (last current-path)
                           next-nodes (get-next current-node)
                           next-nodes (filter (partial valid-next current-path) next-nodes)
                           next-paths (map (partial conj current-path) next-nodes)]
                          (cond (nil? current-path)    routes
                                (= current-node "end") (recur (rest current-paths) (conj routes current-path))
                                :else                  (recur (concat next-paths (rest current-paths)) routes))))]
       (->> (navigate [["start"]] []) count)))

(defn day12b [args]
  (let [connections (->> args 
                         (str/split-lines)
                         (map #(str/split % #"-"))
                         (mapcat (fn [[a b]] [[a b] [b a]]))
                         (group-by first)
                         (map (fn [[a b]] [a (map second b)])))
        get-next (fn [node] (->> connections (filter #(= (first %) node)) first second))
        upper-case? #(= % (str/upper-case %))
        lower-case? #(and ((complement upper-case?) %) ((complement #{"start" "end"}) %))
        visited-small-cave-twice? (fn [visited] (->> visited
                                                     (filter lower-case?)
                                                     frequencies
                                                     (some (fn [[_k v]] (> v 1)))))
        valid-next (fn [visited next] (or (upper-case? next)
                                          (not-in? visited next)
                                          (and (lower-case? next) 
                                               (not (visited-small-cave-twice? visited)))))
        navigate (fn [current-paths routes]
                     (let [current-path (first current-paths)
                           current-node (last current-path)
                           next-nodes   (get-next current-node)
                           next-nodes   (filter (partial valid-next current-path) next-nodes)
                           next-paths   (map (partial conj current-path) next-nodes)]
                     (cond (nil? current-path)    routes
                           (= current-node "end") (recur (rest current-paths) (conj routes current-path))
                           :else                  (recur (concat next-paths (rest current-paths)) routes))))]
       (->> (navigate [["start"]] []) count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 13
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn draw13 [dots] (let [max-x (->> dots (map first) (apply max))
                          max-y (->> dots (map second) (apply max))]
                         (for [y (inclusive-range max-y)
                               x (inclusive-range max-x)
                               :let [suff (if (= x max-x) "\n" "")]]
                              (str (if (in? dots [x y]) "#" ".") suff))))
(defn print13 [x] (when false (println x)) x)

(defn day13 [args]
  (let [[dots folds] (-> args (str/split #"\n\n"))
        dots         (->> dots (str/split-lines) (map #(str/split % #",")) (two-dimensional-mapv parse-int))
        folds        (->> folds 
                          (str/split-lines) 
                          (map #(re-find #"fold along (.)=(\d+)" %)) 
                          (map (fn [[_ fold-axis fold-val]] [fold-axis (parse-int fold-val)])))
        fold-dot     (fn [fold-axis fold-val [x y]]
                         (cond (and (= "x" fold-axis) (> x fold-val)) [(- (* 2 fold-val) x) y]
                               (and (= "y" fold-axis) (> y fold-val)) [x (- (* 2 fold-val) y)]
                               :else                                  [x y]))
        fold         (fn [dots [fold-axis fold-val]]
                         (map (partial fold-dot fold-axis fold-val) dots))
        folds        [(first folds)]]
       (->> (reduce fold dots folds) set count)))

(defn day13b [args]
  (let [[dots folds] (-> args (str/split #"\n\n"))
        dots         (->> dots (str/split-lines) (map #(str/split % #",")) (two-dimensional-mapv parse-int))
        folds        (->> folds 
                          (str/split-lines) 
                          (map #(re-find #"fold along (.)=(\d+)" %)) 
                          (map (fn [[_ fold-axis fold-val]] [fold-axis (parse-int fold-val)])))
        fold-dot     (fn [fold-axis fold-val [x y]]
                         (cond (and (= "x" fold-axis) (> x fold-val)) [(- (* 2 fold-val) x) y]
                               (and (= "y" fold-axis) (> y fold-val)) [x (- (* 2 fold-val) y)]
                               :else                                  [x y]))
        fold         (fn [dots [fold-axis fold-val]]
                         (map (partial fold-dot fold-axis fold-val) dots))]
       (->> (reduce fold dots folds) draw13 print13)))

