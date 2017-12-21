(ns advent.day21
  (:require [clojure.spec.alpha :as s]
            [clojure.test :as test]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.rfs :as xrf]
            [net.cgrand.xforms.io :as xio]
            [criterium.core :as cc]))

(def example
  "../.# => ##./#../...
.#./..#/### => #..#/..../..../#..#")

(defn parse-to-square [s]
  (let [rows (str/split s #"/")]
    (mapv
     (fn [row]
       (into [] row))
     rows)))

(defn parse [in]
  (into {}
        (map (fn [line]
               (let [[from to] (str/split line #" => ")]
                 [(parse-to-square from)
                  (parse-to-square to)])))
        (xio/lines-in in)))


(defn stream [image two-or-three]
  (into []
        (comp (x/partition two-or-three two-or-three)
              (mapcat (fn [rows]
                        (->> (map
                              (fn [row]
                                (partition two-or-three two-or-three row))
                              rows)
                             (apply map vector)))))
        image))

(defn destream [blocks]
  (let [two-or-three (count (first blocks))
        size (long (Math/sqrt (count blocks)))]
    (->> blocks
         (partition size size)
         (mapcat (fn [row-blocks]
                   (reduce
                    (fn [rows block]
                      (map into rows block))
                    (repeat two-or-three [])
                    row-blocks))))))

(defn rotate [block]
  (apply map (fn [& bs]
               (reverse bs)) block))

(defn flip [block]
  (into []
        (reverse block)))

(defn permutations [block]
  (let [rotations (->> block
                       (iterate rotate)
                       (take 4))]
    (into rotations
          (map flip)
          rotations)))

(defn step [enhancemap image]
  (let [two-or-three (if (zero? (rem (count image) 2))
                       2
                       3)
        blocks (stream image two-or-three)
        enhanced (map (fn [block]
                        (some enhancemap
                              (permutations block)))
                      blocks)]
    (destream enhanced)))

(defn day21 [start enhancemap step-count]
  (->> start
       (iterate (fn [image]
                  (step enhancemap image)))
       (drop step-count)
       first
       (apply concat)
       (filter #{\#})
       count))

(comment
  (parse (java.io.StringReader. example))

  (-> (stream (partition 4 4 (range 16)) 2)
      destream)

  (-> (rotate [[0 1]
               [2 3]])
      flip)

  (flip [[0 1]
         [2 3]])

  (rotate [[0 1 2]
           [3 4 5]
           [6 7 8]])

  (flip [[0 1 2]
         [3 4 5]
         [6 7 8]])
  
  (->> (permutations [[\# \.]
                      [\. \.]])
       (into #{})
       count)
  
  (permutations [[0 1 2]
                 [3 4 5]
                 [6 7 8]])

  (->> (step (parse (java.io.StringReader. example))
             [(vec ".#.")
              (vec "..#")
              (vec "###")]
             )
       (step (parse (java.io.StringReader. example))))
  
  
  (day21 [(vec ".#.")
          (vec "..#")
          (vec "###")]
         (parse (java.io.StringReader. example))
         2)
  
  (parse (io/resource "day21.txt"))
  (day21 [(vec ".#.")
          (vec "..#")
          (vec "###")]
         (parse (io/resource "day21.txt")) 5)

  (day21 [(vec ".#.")
          (vec "..#")
          (vec "###")]
         (parse (io/resource "day21.txt")) 18)
  )
