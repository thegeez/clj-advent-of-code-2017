(ns advent.day4
  (:require [clojure.spec.alpha :as s]
            [clojure.test :as test]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]))

(defn valid [line]
  (let [words (str/split line #"\s")]
    (reduce
     (fn [seen word]
       (if (contains? seen word)
         (reduced nil)
         (conj seen word)))
     #{}
     words)))

(defn day4 []
  (let [lines (-> (io/resource "day4.txt")
                  slurp
                  str/split-lines)
        ]
    (-> (keep valid lines)
        count)))

(comment
  (day4)
  (str/split "aa bb cc" #"\s")
  )


(defn valid-anagram [line]
  (let [words (str/split line #"\s")]
    (reduce
     (fn [seen word]
       (let [word-set (set word)]
         (if (contains? seen word-set)
           (reduced nil)
           (conj seen word-set))))
     #{}
     words)))

(defn day4-2 []
  (let [lines (-> (io/resource "day4.txt")
                  slurp
                  str/split-lines)
        ]
    (-> (keep valid-anagram lines)
        count)))

(comment
  (day4-2)
  )
