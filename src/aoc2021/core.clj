(ns aoc2021.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn parse-int [x] (Integer/parseInt x))

(defn p1 [args]
  (->> args
       str/split-lines
       (map parse-int)
       (partition 2 1)
       (filter #(apply < %))
       (count)
      ))

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
  
  (p1 (slurp "src/aoc2021/p1-input.txt"))
  )

(defn p1b [args]
  (->> args
       str/split-lines
       (map parse-int)
       (partition 3 1)
       (partition 2 1)
       (map (fn [ts] (map #(apply + %) ts)))
       (filter #(apply < %))
       (count)
       ))

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

  (p1b (slurp "src/aoc2021/p1-input.txt"))
  )