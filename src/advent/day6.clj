(ns advent.day6
  (:require [clojure.spec.alpha :as s]
            [clojure.test :as test]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.io :as xio]))


(defn day6 [in]
  (loop [seen #{}
         n-cycle 0
         memory in]
    (let [[max-idx max-count] (reduce
                               (fn [[midx mc :as ma] [ci cc :as ca]]
                                 (if (< mc cc)
                                   ca
                                   ma))
                               [-1 -1]
                               (map list (range) memory))]
      (let [next-memory (reduce
                         (fn [m idx]
                           (update m (mod idx (count m)) inc))
                         (assoc memory max-idx 0)
                         (range (inc max-idx) (+ 1 max-idx max-count)))]
        (if (contains? seen next-memory)
          (inc n-cycle)
          (recur (conj seen next-memory)
                 (inc n-cycle)
                 next-memory))))))


(defn day6-2 [in]
  (loop [seen-at {}
         n-cycle 0
         memory in]
    (let [[max-idx max-count] (reduce
                               (fn [[midx mc :as ma] [ci cc :as ca]]
                                 (if (< mc cc)
                                   ca
                                   ma))
                               [-1 -1]
                               (map list (range) memory))]
      (let [next-memory (reduce
                         (fn [m idx]
                           (update m (mod idx (count m)) inc))
                         (assoc memory max-idx 0)
                         (range (inc max-idx) (+ 1 max-idx max-count)))]
        (if-let [first-seen (get seen-at next-memory)]
          (- (inc n-cycle) first-seen)
          (recur (assoc seen-at next-memory (inc n-cycle))
                 (inc n-cycle)
                 next-memory))))))

(comment
  (day6 [0 2 7 0])
  (day6-2 [0 2 7 0])
  
  (def in (-> (io/resource "day6.txt")
              (slurp)
              str/trim
              str/split-lines
              first
              (str/split #"\t")
              (->> (mapv #(Long/parseLong %)))))

  (day6 in)
  (day6-2 in)

  )
