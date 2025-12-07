(ns aoc.2025.day-05
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn read-chunks
  "Return file contents as collection of chunks, 
   where chunks are separated by a full blank line."
  [path]
  (-> path
      slurp
      str/trim-newline
      (str/split #"\n\n")))

(defn ints-pos
  "Return a collection of positive integers found in a string.  Useful for strings with
  hyphens instead of negative signs."
  [s]
  (mapv read-string (re-seq #"\d+" s)))

(defn parse-fresh [s]
  (->> s
       str/split-lines
       (mapv ints-pos)))


(defn parse-input [path]
  (let [chunks (read-chunks path)
        fresh (parse-fresh (first chunks))
        available (->> chunks
                       second
                       str/split-lines
                       (mapv parse-long))]
    [fresh available]))

(def test-data (parse-input "resources/inputs/2025/d05-test.txt"))
(def real-data (parse-input "resources/inputs/2025/d05.txt"))

;; ---------------

(defn in-range? [value start end]
  (<= start value end))

(defn in-any-range? [ranges value]
  (some (fn [[start end]] 
          (in-range? value start end)) 
        ranges))

(defn solve-1 [data]
  (let [[fresh available] data
        is-fresh? (partial in-any-range? fresh)]
    (count (filter is-fresh? available))))

;; ---------------

(defn overlap? [[a-lo a-hi][b-lo b-hi]]
  (or (<= a-lo b-lo a-hi)
      (<= a-lo b-hi a-hi)))

(defn merge-range [[a-lo a-hi] [b-lo b-hi]]
  [(min a-lo b-lo) (max a-hi b-hi)])

(defn consolidate-range [acc curr]
  (if (overlap? (last acc) curr)
    (conj (pop acc) (merge-range (last acc) curr))
    (conj acc curr)))

(defn- values-in-range [[lo hi]]
  (inc (- hi lo)))

;; ---------------

(defn solve-2 [data]
  (let [[r & rs] (sort-by first (first data))]
    (->> rs
         (reduce consolidate-range [r])
         (map values-in-range)
         (reduce +))))


(deftest aoc-2025.day-5
    (testing solve-1
      (is (= 3   (solve-1 test-data)))
      (is (= 773 (solve-1 real-data))))
    (testing solve-2
      (is (= 14   (solve-2 test-data)))
      (is (= 332067203034711 (solve-2 real-data)))))

(clojure.test/run-tests)