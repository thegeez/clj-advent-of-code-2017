(ns advent.day22
  (:require [clojure.spec.alpha :as s]
            [clojure.test :as test]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.rfs :as xrf]
            [net.cgrand.xforms.io :as xio]
            [criterium.core :as cc]))

(def example
  "..#
#..
...")


(defn parse [in]
  (let [rows (into []
                   (xio/lines-in in))
        _ (assert (and (odd? (count rows))
                       (count (first rows))))
        middle (/ (dec (count rows)) 2)
        infected (into #{}
                       (comp (map-indexed list)
                             (x/for [[ri row] %
                                     ci (range (count row))
                                     :when (= (get row ci) \#)]
                               [ri ci]))
                       rows)]
    {:grid infected
     :dir [-1 0]
     :at [middle middle]
     :infections 0}))

(def clockwise [[-1 0] ;;up
                [ 0 1] ;; right
                [1 0] ;; down
                [0 -1] ;; left
                ])

(def turn-right
  (zipmap clockwise
          (rest (cycle clockwise))))

(def turn-left
  (zipmap (vals turn-right)
          (keys turn-right)))

(defn step [world]
  (let [{:keys [grid dir at infections]} world
        at-infected (get grid at)
        dir (if at-infected
              (turn-right dir)
              (turn-left dir))
        infect (if at-infected
                 false
                 true)]
    {:grid (if infect
             (conj grid at)
             (disj grid at))
     :dir dir
     :at (map + at dir)
     :infections (if infect
                   (inc infections)
                   infections)}))

(defn draw [world]
  (let [{:keys [grid at dir]} world
        from-width (apply min (second at) (map second grid))
        to-width (apply max (second at) (map second grid))
        from-height (apply min (first at) (map first grid))
        to-height (apply max (first at) (map first grid))
        from-size (min from-width from-height)
        to-size (inc (max to-width to-height))]
    (println "GRID: " grid)
    (println "FACING: " (:dir world))
    (println "AT: " (:at world))
    (doseq [ri (range from-size to-size)]
      (println (apply str
                      (for [ci (range from-size to-size)]
                        (let [us (= [ri ci] at)]
                          (str (if us
                                 \[
                                 \space)
                               (if-let [node (get grid [ri ci])]
                                 \#
                                 \.)
                               (if us \] \space)))))))
    world))

(defn day22 [config]
  (->> config
       (iterate step)
       #_(map draw)
       (drop 10000)
       first
       #_draw
       :infections))

(defn step2 [world]
  (let [{:keys [grid dir at infections]} world
        at-node (get grid at)
        dir (case at-node
              :weakened
              dir
              :infected
              (turn-right dir)
              :flagged
              (map * [-1 -1] dir)
              ;; nil = clean
              (turn-left dir)
              )
        todo (case at-node
               :infected :flagged
               :weakened :infected
               :flagged :clean
               ;; clean becomes
               :weakened
               )]
    {:grid (if (= todo :clean)
             (dissoc grid at)
             (assoc grid at todo))
     :dir dir
     :at (map + at dir)
     :infections (if (= todo :infected)
                   (inc infections)
                   infections)}))

(defn day22-2 [config]
  (->> (update config :grid
               (fn [node-set]
                 (zipmap (seq node-set)
                         (repeat :infected))))
       (iterate step2)
       #_(map draw)
       (drop 10000000)
       first
       #_draw
       :infections))



(comment
  (parse (java.io.StringReader. example))
  (day22 (parse (java.io.StringReader. example)))
  (day22 (parse (io/resource "day22.txt")))

  (day22-2 (parse (java.io.StringReader. example)))
  (day22-2 (parse (io/resource "day22.txt")))
  )
