(ns advent.day2
  (:require [clojure.spec.alpha :as s]
            [clojure.test :as test]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]))

(defn day2 [in-str]
  (let [lines (str/split-lines in-str)
        rows (map (fn [line]
                    (str/split line #"\s"))
                  lines)
        num-rows (map (fn [row]
                        (map (fn [cn] (Long/parseLong cn)) row)) rows)
        res (transduce
             (map (fn [nums]
                    (transduce
                     (comp (x/transjuxt {:min x/min
                                         :max x/max})
                           (map (fn [{:keys [min max]}]
                                  (- max min))))
                     + ;; return single number result
                     nums)))
             +
             num-rows)]
    res))

(test/deftest tests
  (test/are [in out] (= (day2 in) out)
    "5 1 9 5
7 5 3
2 4 6 8" 18))

(comment
  (tests)
  (day2 (slurp (io/resource "day2.txt")))

  )


(defn day2-2 [in-str]
  (let [lines (str/split-lines in-str)
        rows (map (fn [line]
                    (str/split line #"\s"))
                  lines)
        num-rows (map (fn [row]
                        (map (fn [cn] (Long/parseLong cn)) row)) rows)
        divs (map (fn [nums]
                    (first (for [[n & ns] (iterate next nums)
                                 :while ns
                                 m ns
                                 :while m
                                 :let [[high low] (if (< n m)
                                                    [m n]
                                                    [n m])]
                                 :when (zero? (mod high low))]
                             (/ high low))))
                  num-rows)
        res (reduce + divs)]
    res))

(test/deftest tests2
  (test/are [in out] (= (day2-2 in) out)
    "5 9 2 8
9 4 7 3
3 8 6 5" 9))

(comment
  (tests2)
  (day2-2 (slurp (io/resource "day2.txt")))
  )
