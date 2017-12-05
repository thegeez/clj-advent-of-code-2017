(ns advent.day5
  (:require [clojure.spec.alpha :as s]
            [clojure.test :as test]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]))

(defn data []
  (->> (io/resource "day5.txt")
       slurp
       str/split-lines
       (mapv #(Long/parseLong %))))

(defn day5 [arr]
  (loop [steps 0
         i 0
         arr arr]
    (if-let [jump (get arr i)]
      (recur (inc steps)
             (+ i jump)
             (update arr i inc))
      steps)))

(defn day5-2 [arr]
  (loop [steps 0
         i 0
         arr arr]
    (if-let [jump (get arr i)]
      (recur (inc steps)
             (+ i jump)
             (update arr i
                     (fn [x]
                       (if (<= 3 x)
                         (dec x)
                         (inc x)))))
      steps)))

(comment
  (data)
  (day5 [0
         3
         0
         1
         -3])
  
  (day5 (data))


  (day5-2 [0
           3
           0
           1
           -3])
  (day5-2 (data))
  )
