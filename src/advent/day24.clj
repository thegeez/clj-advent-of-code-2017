(ns advent.day24
  (:require [clojure.spec.alpha :as s]
            [clojure.test :as test]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.rfs :as xrf]
            [net.cgrand.xforms.io :as xio]
            [criterium.core :as cc]))

(def example
  "0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10")

(defn parse [in]
  (into []
        (map (fn [line]
               (->> (str/split line #"/")
                    (mapv #(Long/parseLong %)))))
        (xio/lines-in in)))

(defn make-paths [pcs]
  (loop [[t & todo] [[[[0 0]] (set pcs)]]
         paths #{}]
    (if-not t
      paths
      (let [[path avail] t
            connect (peek (peek path))
            choices (reduce
                     (fn [choices [l r :as piece]]
                       (if (= l connect)
                         (conj choices
                               [(conj path [l r]) (disj avail piece)])
                         (if (= r connect)
                           (conj choices
                                 [(conj path [r l]) (disj avail piece)])
                           choices)))
                     []
                     avail)]
        (if (seq choices)
          (recur (into todo
                       choices)
                 paths)
          (recur todo
                 (conj paths path))
          )))))

(defn day24 [pcs]
  (let [paths (make-paths pcs)]
    (apply max (map
                (fn [path]
                  (reduce + (mapcat identity path)))
                paths))))

(defn day24-2 [pcs]
  (let [paths (make-paths pcs)
        [length longest] (reduce
                          (fn [[length bests :as acc] path]
                            (let [l (count path)]
                              (if (< length l)
                                [l [path]]
                                (if (= length l)
                                  [length (conj bests path)]
                                  acc))))
                          [-1 []]
                          paths)]
    (apply max (map
                (fn [path]
                  (reduce + (mapcat identity path)))
                longest))))

(comment

  (def pcs (parse (io/resource "day24.txt")))
  (count pcs)
  (count (distinct pcs))
  (count (into []
               (map (fn [[l r]]
                      (if (< l r)
                        [l r]
                        [r l])))
               pcs))

  ;; no doubles, or reverse ones

  (day24 (parse (java.io.StringReader. example)))
  (day24 pcs)

  (day24-2 (parse (java.io.StringReader. example)))
  (day24-2 pcs)
  )
