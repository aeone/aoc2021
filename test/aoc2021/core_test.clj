(ns aoc2021.core-test
  (:require [clojure.test :refer :all]
            [aoc2021.core :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day1-sample-input "199
200
208
210
200
207
240
269
260
263")

(deftest day1-test
  (testing "Day 1 sample input"
    (is (= 7 
           (day1 day1-sample-input)))))

(comment
  (day1 (slurp "src/aoc2021/day1-input.txt")))

(deftest day1b-test
  (testing "Day 1b sample input"
    (is (= 5 
           (day1b day1-sample-input)))))

(comment
  (day1b (slurp "src/aoc2021/day1-input.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day2-sample-input "forward 5
down 5
forward 8
up 3
down 8
forward 2")

(deftest day2-test
  (testing "Day 2 sample input"
    (is (= 150 
           (day2 day2-sample-input)))))

(comment
  (day2 (slurp "src/aoc2021/day2-input.txt")))

(deftest day2b-test
  (testing "Day 2b sample input"
    (is (= 900 
           (day2b day2-sample-input)))))

(comment
  (day2b (slurp "src/aoc2021/day2-input.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day3-sample-input "00100
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

(deftest day3-test
  (testing "Day 3 sample input"
    (is (= 198
           (day3 day3-sample-input)))))

(comment
  (day3 (slurp "src/aoc2021/day3-input.txt")))

(deftest day3b-test
  (testing "Day 3b sample input"
    (is (= 230
           (day3b day3-sample-input)))))

(comment
  (apply max-key val {0 5 1 5})
  (day3b (slurp "src/aoc2021/day3-input.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day4-sample-input "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

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
 2  0 12  3  7")

(deftest day4-test
  (testing "Day 4 sample input"
    (is (= 4512
           (day4 day4-sample-input)))))

(comment
  (day4 (slurp "src/aoc2021/day4-input.txt")))

(deftest day4b-test
  (testing "Day 4b sample input"
    (is (= 1924
           (day4b day4-sample-input)))))

(comment
  (day4b (slurp "src/aoc2021/day4-input.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day5-sample-input "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(deftest day5-test
  (testing "Day 5 sample input"
    (is (= 5
           (day5 day5-sample-input)))))

(comment
  (day5 (slurp "src/aoc2021/day5-input.txt")))

(deftest day5b-test
  (testing "Day 5b sample input"
    (is (= 12
           (day5b day5-sample-input)))))

(comment
  (day5b (slurp "src/aoc2021/day5-input.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day6-sample-input "3,4,3,1,2")

(deftest day6-test
  (testing "Day 6 sample input"
    (is (= 5934
           (day6 day6-sample-input)))))

(comment
  (day6 (slurp "src/aoc2021/day6-input.txt")))

(deftest day6b-test
  (testing "Day 6b sample input"
    (is (= 26984457539
           (day6b day6-sample-input)))))

(comment
  (day6b (slurp "src/aoc2021/day6-input.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day7-sample-input "16,1,2,0,4,2,7,1,2,14")

(deftest day7-test
  (testing "Day 7 sample input"
    (is (= 37
           (day7 day7-sample-input)))))

(comment
  (time (day7 (slurp "src/aoc2021/day7-input.txt"))))

(deftest day7b-test
  (testing "Day 7b sample input"
    (is (= 168
           (day7b day7-sample-input)))))

(comment
  (time (day7b (slurp "src/aoc2021/day7-input.txt"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day8-sample-input "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(deftest day8-test
  (testing "Day 8 sample input"
    (is (= 26
           (day8 day8-sample-input)))))

(comment
  (time (day8 (slurp "src/aoc2021/day8-input.txt"))))

(deftest day8b-test
  (testing "Day 8b sample input"
    (is (= 61229
           (day8b day8-sample-input)))))

(comment
  (time (day8b (slurp "src/aoc2021/day8-input.txt"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 9
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day9-sample-input "2199943210
3987894921
9856789892
8767896789
9899965678")

(deftest day9-test
  (testing "Day 9 sample input"
    (is (= 15
           (day9 day9-sample-input)))))

(comment
  (time (day9 (slurp "src/aoc2021/day9-input.txt"))))

(deftest day9b-test
  (testing "Day 9b sample input"
    (is (= 1134
           (day9b day9-sample-input)))))

(comment
  (time (day9b (slurp "src/aoc2021/day9-input.txt"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day10-sample-input "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")

(deftest day10-test
  (testing "Day 10 sample input"
    (is (= 26397
           (day10 day10-sample-input)))))

(comment
  (time (day10 (slurp "src/aoc2021/day10-input.txt"))))

(deftest day10b-test
  (testing "Day 10b sample input"
    (is (= 288957
           (day10b day10-sample-input)))))

(comment
  (time (day10b (slurp "src/aoc2021/day10-input.txt"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day11-sample-input "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")

(deftest day11-test
  (testing "Day 11 sample input"
    (is (= 1656
           (day11 day11-sample-input)))))

(comment
  (time (day11 (slurp "src/aoc2021/day11-input.txt"))))

(deftest day11b-test
  (testing "Day 11b sample input"
    (is (= 195
           (day11b day11-sample-input)))))

(comment
  (time (day11b (slurp "src/aoc2021/day11-input.txt"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 12
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day12-sample-input-1 "start-A
start-b
A-c
A-b
b-d
A-end
b-end")
(def day12-sample-input-2 "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW")

(deftest day12-test-1
  (testing "Day 12 sample input 1"
    (is (= 10
           (day12 day12-sample-input-1)))))
(deftest day12-test-2
  (testing "Day 12 sample input 2"
    (is (= 226
           (day12 day12-sample-input-2)))))

(comment
  (time (day12 (slurp "src/aoc2021/day12-input.txt"))))

(deftest day12b-test-1
  (testing "Day 12b sample input 1"
    (is (= 36
           (day12b day12-sample-input-1)))))
(deftest day12b-test-2
  (testing "Day 12b sample input 2"
    (is (= 3509
           (day12b day12-sample-input-2)))))

(comment
  (time (day12b (slurp "src/aoc2021/day12-input.txt"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Day 13
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day13-sample-input "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5")

(deftest day13-test
  (testing "Day 13 sample input"
    (is (= 17
           (day13 day13-sample-input)))))

(comment
  (time (day13 (slurp "src/aoc2021/day13-input.txt"))))

(deftest day13b-test
  (testing "Day 13b sample input"
    (is (= '("#" "#" "#" "#" "#\n" 
             "#" "." "." "." "#\n" 
             "#" "." "." "." "#\n" 
             "#" "." "." "." "#\n" 
             "#" "#" "#" "#" "#\n")
           (day13b day13-sample-input)))))

(comment
  (time (day13b (slurp "src/aoc2021/day13-input.txt"))))
