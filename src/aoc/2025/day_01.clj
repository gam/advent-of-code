(ns aoc.2025.day-01
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn parse-line [s]
  (parse-long (str/escape s {\L \- \R \+})))

(defn parse-input [filename]
  (->> filename
       slurp
       str/split-lines
       (map parse-line)))

(def test-data (parse-input "resources/inputs/2025/d01-test.txt"))
(def real-data (parse-input "resources/inputs/2025/d01.txt"))

;; ---------------

(defn solve-1 [[curr zeros] delta]
  (let [result (+ curr delta)
        dial (mod result 100)]
    [dial (if (zero? dial) (inc zeros) zeros)]))

;; ---------------

(defn solve-2 [[acc zeros] delta]
  (let [result (+ acc delta)
        rotations (abs (quot result 100))
        nzeros (if (or (zero? result)
                       (and (pos? acc) (neg? result))) (inc rotations) rotations)]
    [(mod result 100) (+ nzeros zeros)]))

;; ---------------

(defn solver [method data]
  (second (reduce method [50 0] data)))

;; ---------------

(deftest aoc-2025.day1
  (testing solve-1
    (is (= 3 (solver solve-1 test-data)))
    (is (= 1074(solver solve-1 real-data))))
  (testing solve-2
    (is (= 6 (solver solve-2 test-data)))
    (is (= 6254 (solver solve-2 real-data)))))

(clojure.test/run-tests)