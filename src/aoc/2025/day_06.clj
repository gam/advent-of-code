(ns aoc.2025.day-06
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn fetch-input [filename]
  (->> filename
       slurp
       str/split-lines
       reverse))

(defn parse-numbers [s]
  (->> s
       (re-seq #"\d+")
       (map parse-long)))

(defn parse-ops [s]
  (->> s
       (re-seq #"[+*]")
       (map {"*" #'*
             "+" #'+})))

(defn parse-input [data]
  (let [[ops & nums] data]
    (cons (parse-ops ops)
          (map parse-numbers nums))))

(def test-data (parse-input (fetch-input "resources/inputs/2025/d06-test.txt")))
(def real-data (parse-input (fetch-input "resources/inputs/2025/d06.txt")))

;; ---------------

(defn calc-slice [[op & numbers]]
  (apply op numbers))

(defn solve-1 [data]
  (->> data
       (apply map list)
       (map calc-slice)
       (reduce +)))

;; ---------------

(defn fetch-input-2 [filename]
  (->> filename
       slurp
       str/split-lines))

(defn parse-lengths [s]
  (map count (re-seq #"[+*]\s+" s)))

(defn chop-string [[acc s] curr]
  [(conj acc (take curr s))
   (drop curr s)])

(defn make-chunks [lengths s]
  (first (reduce chop-string
                 [[] s]
                 lengths)))

(defn- ->int [xs]
  (->> xs
       (apply str)
       str/trim
       parse-long))

(defn parse-chunk [chunk]
  (->> chunk
       (apply mapv vector)
       (map ->int)
       (remove nil?)))

(defn process-input-2 [data]
  (let [nums (butlast data)
        lengths (parse-lengths (last data))
        operations (parse-ops (last data))
        chunks (map (partial make-chunks lengths) nums)]
    {:nums (->> chunks
                (apply mapv vector)
                (map parse-chunk))
     :ops operations}))

;; ---------------

(def test-data-2 (process-input-2 (fetch-input-2 "resources/inputs/2025/d06-test.txt")))
(def real-data-2 (process-input-2 (fetch-input-2 "resources/inputs/2025/d06.txt")))

;; ---------------

(defn solve-2 [{:keys [nums ops]}]
  (reduce + (map apply ops nums)))

;; ---------------

(deftest aoc-2025.day-6
  (testing solve-1
    (is (= 4277556   (solve-1 test-data)))
    (is (= 5227286044585 (solve-1 real-data))))
  (testing solve-2
      (is (= 3263827   (solve-2 test-data-2)))
      (is (= 10227753257799 (solve-2 real-data-2)))))

(clojure.test/run-tests)