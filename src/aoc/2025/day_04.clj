(ns aoc.2025.day-04
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn- parse-line [s] 
  (str/split s #""))

(defn parse-input [filename]
  (->> filename
       slurp
       str/split-lines
       (mapv parse-line)))

(def test-data1 (parse-input "resources/inputs/2025/d04-test1.txt"))
(def test-data2 (parse-input "resources/inputs/2025/d04-test2.txt"))
(def real-data (parse-input "resources/inputs/2025/d04.txt"))

;; ---------------

(defn find-rolls
  "Given a two-dimensional matrix of strings, 
   find the set of coordinates for cells with the value '@'"
  [matrix]
  (set
   (for [[i vals] (map-indexed vector matrix)
         [j val] (map-indexed vector vals)
         :when (= "@" val)]
     [i j])))

(defn neighbours
  "Given a cell's coordinates, returns the coordinates of its neighbours."
  [[x y]]
  (for [dx [-1 0 1]
        dy (if (zero? dx) [-1 1] [-1 0 1])]
    [(+ dx x) (+ dy y)]))

(defn accessible?
  "Given a set of roll coordinates, check if a roll has less than four adjacent rolls."
  [rolls roll]
  (->> roll
       neighbours
       (filter (set rolls))
       count
       (> 4)))

;; ---------------

(defn solve-1 [data]
  (let [rolls (find-rolls data)]
    (count (filter (partial accessible? rolls) rolls))))

;; ---------------

(defn solve-2 [data]
  (loop [rolls (find-rolls data)
         removed 0]
    (let [accessible (filter (partial accessible? rolls) rolls)]
      (println "removed: " removed)
      (if (empty? accessible)
        removed
        (recur (remove (set accessible) rolls) (+ removed (count accessible)))))))

;; ---------------

(deftest aoc-2025.day-4
  (testing solve-1
    (is (= 13   (solve-1 test-data1)))
    (is (= 12   (solve-1 test-data2)))
    (is (= 1464 (solve-1 real-data))))
  (testing solve-2
    (is (= 43   (solve-2 test-data1)))
    (is (= 30   (solve-2 test-data2)))
    #_(is (= 8409 (solve-2 real-data)))))

(clojure.test/run-tests)