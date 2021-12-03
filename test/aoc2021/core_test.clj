(ns aoc2021.core-test
  (:require [clojure.test :refer :all]
            [aoc2021.core :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest day1a
  (testing "Day 1 sample input"
    (is (= 7 
           (p1 "199
200
208
210
200
207
240
269
260
263")))))

(comment
  (p1 (slurp "src/aoc2021/p1-input.txt")))

(deftest day1b
  (testing "Day 1b sample input"
    (is (= 5 
           (p1b "199
200
208
210
200
207
240
269
260
263")))))

(comment
  (p1b (slurp "src/aoc2021/p1-input.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest day2
  (testing "Day 2 sample input"
    (is (= 150 
           (p2 "forward 5
down 5
forward 8
up 3
down 8
forward 2")))))

(comment
  (p2 (slurp "src/aoc2021/p2-input.txt")))

(deftest day2b
  (testing "Day 2b sample input"
    (is (= 900 
           (p2b "forward 5
down 5
forward 8
up 3
down 8
forward 2")))))

(comment
  (p2b (slurp "src/aoc2021/p2-input.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest day3
  (testing "Day 3 sample input"
    (is (= 198
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
01010")))))

(comment
  (p3 (slurp "src/aoc2021/p3-input.txt")))

(deftest day3b
  (testing "Day 3b sample input"
    (is (= 230
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
01010")))))

(comment
  (apply max-key val {0 5 1 5})
  (p3b (slurp "src/aoc2021/p3-input.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

