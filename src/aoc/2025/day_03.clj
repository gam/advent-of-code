(ns aoc.2025.day-03
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))


(defn parse-line [s]
  (mapv parse-long (str/split s #"")))

(defn parse-input [filename]
  (->> filename
       slurp
       str/split-lines
       (map parse-line)))

(def test-data (parse-input "resources/inputs/2025/d03-test.txt"))
(def real-data (parse-input "resources/inputs/2025/d03.txt"))

;; ---------------

(defn maximise-jolt [[tens ones] x]
  (cond
    (> ones tens) [ones x]
    (> x ones)    [tens x]
    :else [tens ones]))

(defn solve-1 [bank]
  (->> bank
       (reduce maximise-jolt [0 0])
       (apply str)
       parse-long))

;; ---------------

(defn increase-jolt [batteries]
  (loop [acc []
         [num & nums] batteries]
    (if nums
      (if (> (first nums) num)
        (into acc nums)
        (recur (conj acc num) nums))
      (conj acc num))))

(defn maximise-jolt-2 [acc x]
  (let [acc' (increase-jolt acc)]
    (if (> (count acc)
           (count acc'))
      (conj acc' x)
      (if (> x (last acc))
        (conj (pop acc') x)
        acc))))

(defn solve-2 [bank]
  (parse-long 
   (apply str 
          (reduce maximise-jolt-2 (take 12 bank) (drop 12 bank)))))

;; ---------------

(defn solver [method data]
  (reduce + (map method data)))

;; ---------------

(deftest aoc-2025.day-3
  (testing solve-1
    (is (= 357  (solver solve-1 test-data)))
    (is (= 17403 (solver solve-1 real-data))))
  (testing solve-2
    (is (= 3121910778619  (solver solve-2 test-data)))
    (is (= 173416889848394 (solver solve-2 real-data)))))

(clojure.test/run-tests)