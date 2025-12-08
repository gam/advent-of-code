(ns aoc.2025.day-07
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.set :as set]))

(def start-or-splitter #{\S \^})

(defn- find-specials [index value]
  (when (start-or-splitter value) index))

(defn- parse-row [s]
  (->> s
       (keep-indexed find-specials)
       set))

(defn parse-input
  [path]
  (->> path
       slurp
       str/split-lines
       (map parse-row)
       (remove empty?)))

(def test-data (parse-input "resources/inputs/2025/d07-test.txt"))
(def real-data (parse-input "resources/inputs/2025/d07.txt"))

;; ---------------

(defn- make-col-splitter [splitters-in-row]
  (fn [col]
    (if (splitters-in-row col)
      [(dec col) (inc col)]
      [col])))

(defn solve-1 [data]
  (let [[start & splitters] data] 
    (loop [[splitters-in-row & remaining] splitters
           n 0 
           cols start]
      (if (nil? splitters-in-row)
        n
        (recur remaining
               (+ n (count (set/intersection cols splitters-in-row)))
               (->> cols
                    (mapcat (make-col-splitter splitters-in-row))
                    set))))))

;; ---------------

(defn- make-split-parser [splitters-in-row]
  (fn [[col timeline-count]]
    (if (splitters-in-row col)
      [[(dec col) timeline-count] [(inc col) timeline-count]]
      [[col timeline-count]])))

(defn- merge-column-timelines [acc [col timeline-count]]
  (update acc col (fnil + 0) timeline-count))

(defn solve-2 [data]
  (let [[start & splitters] data] 
    (loop [[splitters-in-row & remaining] splitters
           beams {(first start) 1}]  
      (if (nil? splitters-in-row)    
        (reduce + (vals beams))      
        (recur remaining (->> beams
                              (mapcat (make-split-parser splitters-in-row))
                              (reduce merge-column-timelines {})))))))

;; ---------------

(defn solver [method data]
  (method data))

(deftest aoc-2025.day7
    (testing solve-1
      (is (= 21 (solver solve-1 test-data)))
      (is (= 1587 (solver solve-1 real-data))))
    (testing solve-2
      (is (= 40 (solver solve-2 test-data)))
      (is (= 5748679033029 (solver solve-2 real-data)))))

(clojure.test/run-tests)