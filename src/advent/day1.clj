(ns advent.day1
  (:require [clojure.spec.alpha :as s]
            [clojure.test :as test]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]))

(defn day1 [in-str]
  (let [in-nums (sequence (map (fn [c]
                                 (Long/parseLong (str c))))
                          in-str)]
    (transduce
       (comp (x/partition 2 1 [(first in-nums)])
             (keep (fn [[l r]]
                     (when (= l r)
                       l))))
       +
       in-nums)))

(test/deftest tests
  (test/are [in out] (= (day1 in) out)
    "1122" 3
    "1111" 4
    "1234" 0
    "91212129" 9))

(comment
  (tests)
  ;; new line at the end of file breaks long parsing
  (day1 (first (line-seq (io/reader (io/resource "day1.txt")))))
  ;;1203
  )


(defn day1-2 [in-str]
  (let [in-nums (sequence (map (fn [c]
                                 (Long/parseLong (str c))))
                          in-str)
        partition-size (inc (/ (count in-nums) 2))]
    (->> in-nums
         (partition-all partition-size 1)
         (keep (fn [nums]
                 (let [nums (if (< (count nums) partition-size)
                              (take partition-size (concat nums in-nums))
                              nums)]
                   (when (= (first nums) (last nums))
                     (first nums)))))
         (reduce +))))

(test/deftest test2
  (test/are [in out] (= (day1-2 in) out)
    "1212" 6
    "1221" 0
    "123425" 4
    "123123" 12
    "12131415" 4
    ))

(partition-all 3 1 "1234")
(comment
  (test2)
  (day1-2 (first (line-seq (io/reader (io/resource "day1.txt")))))
  )
