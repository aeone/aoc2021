(ns aoc2021.core-test
  (:require [clojure.test :refer :all]
            [aoc2021.core :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest day1-test
  (testing "Day 1 sample input"
    (is (= 7 
           (day1 "199
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
  (day1 (slurp "src/aoc2021/day1-input.txt")))

(deftest day1b-test
  (testing "Day 1b sample input"
    (is (= 5 
           (day1b "199
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
  (day1b (slurp "src/aoc2021/day1-input.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest day2-test
  (testing "Day 2 sample input"
    (is (= 150 
           (day2 "forward 5
down 5
forward 8
up 3
down 8
forward 2")))))

(comment
  (day2 (slurp "src/aoc2021/day2-input.txt")))

(deftest day2b-test
  (testing "Day 2b sample input"
    (is (= 900 
           (day2b "forward 5
down 5
forward 8
up 3
down 8
forward 2")))))

(comment
  (day2b (slurp "src/aoc2021/day2-input.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest day3-test
  (testing "Day 3 sample input"
    (is (= 198
           (day3 "00100
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
  (day3 (slurp "src/aoc2021/day3-input.txt")))

(deftest day3b-test
  (testing "Day 3b sample input"
    (is (= 230
           (day3b "00100
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
  (day3b (slurp "src/aoc2021/day3-input.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest day4-test
  (testing "Day 4 sample input"
    (is (= 4512
           (day4 "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")))))

(comment
  (day4 (slurp "src/aoc2021/day4-input.txt")))

(deftest day4b-test
  (testing "Day 4b sample input"
    (is (= 1924
           (day4b "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")))))

(comment
  (day4b (slurp "src/aoc2021/day4-input.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest day5-test
  (testing "Day 5 sample input"
    (is (= 5
           (day5 "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")))))

(comment
  (day5 (slurp "src/aoc2021/day5-input.txt")))