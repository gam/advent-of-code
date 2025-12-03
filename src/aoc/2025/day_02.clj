(ns aoc.2025.day-02
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn split-line [s]
  (str/split s #","))

(defn make-pair [s]
  (mapv parse-long (str/split s #"-")))

(defn parse-input [filename]
  (->> filename
       slurp
       split-line
       (map make-pair)))

(def test-data (parse-input "resources/inputs/2025/d02-test.txt"))
(def real-data (parse-input "resources/inputs/2025/d02.txt"))

;; ---------------

(defn invalid? [n]
  (let [nstr (str n)
        length (count nstr)]
    (and (even? length)
         (= (subs nstr 0 (/ length 2))
            (subs nstr (/ length 2))))))

(defn solve-1 [data]
  (for [[start end] data]
    (reduce + (filter invalid? (range start (inc end))))))

;; ---------------

(defn find-invalid-in-pair [[lower upper]]
  (filter #(re-find #"^(.+)\1+$" (str %))
          (range lower (inc upper))))

(defn solve-2 [data]
  (mapcat find-invalid-in-pair data))

;; ---------------

(defn solver [method data]
  (->> data
       method
       (reduce +)))

;; ---------------

(deftest aoc-2025.day-2
  (testing solve-1
    (is (= 1227775554  (solver solve-1 test-data)))
    (is (= 24043483400 (solver solve-1 real-data))))
  (testing solve-2
    (is (= 4174379265  (solver solve-2 test-data)))
    (is (= 38262920235 (solver solve-2 real-data)))))

(clojure.test/run-tests)