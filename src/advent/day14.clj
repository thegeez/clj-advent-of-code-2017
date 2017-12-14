(ns advent.day14
  (:require [clojure.spec.alpha :as s]
            [clojure.test :as test]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.rfs :as xrf]
            [net.cgrand.xforms.io :as xio]
            [criterium.core :as cc]
            [advent.day10 :as d10]))

;; flqrgnkx
;;
;; free = . = 0
;; used = # = 1
;;
;; ##.#.#..-->
;; .#.#.#.#   
;; ....#.#.   
;; #.#.##.#   
;; .##.#...   
;; ##..#..#   
;; .#...#..   
;; ##.#.##.-->
;; |      |   
;; V      V   

(def hex-chars "0123456789abcdef")
(def hex-to-long (zipmap hex-chars
                         (range)))

(defn hex-to-bits [hex-str]
  (into []
        (comp (map hex-to-long)
              (x/partition 2 2)
              (map (fn [[l r]]
                     (bit-or (bit-shift-left l 4)
                             r))))
        hex-str))

(defn key-to-bit-rows [key-stem]
  (map (fn [row-idx]
         (let [row-key (str key-stem "-" row-idx)
               nums (->> (d10/knot-hash-hex row-key)
                         hex-to-bits)
               bits (for [n nums
                          b (range 7 -1 -1)]
                      (if (bit-test n b)
                        1
                        0))]
           bits))))

(defn day14 [key-stem]
  (transduce
   (comp (key-to-bit-rows key-stem)
         cat)
   +
   (range 128)))

(comment
  (d10/knot-hash-hex "AoC 2017")
  ;; "33efeb34ea91902bb2f59c9920caa6cd"

  (d10/knot-hash-hex "flqrgnkx-0")
  ;; "d4f76bdcbf838f8416ccfa8bc6d1f9e6"

  (day14 "flqrgnkx")

  (day14 "xlqgujun"))

(defn day14-2 [key-stem]
  (let [bit-rows (into []
                       (comp (key-to-bit-rows key-stem)
                             (map vec))
                       (range 128))
        width (count bit-rows)
        height (count (first bit-rows))
        neighbors (into {}
                        (for [i (range height)
                              j (range width)
                              :let [node-id (+ (* i height)
                                               j)]
                              :when (= (get-in bit-rows [i j]) 1)]
                          [node-id (for [[di dj]  [        [0 -1]
                                                   [-1  0]        [1  0]
                                                   [0  1]]
                                         :let [ni (+ i di)
                                               nj (+ j dj)]
                                         :when (= (get-in bit-rows [ni nj]) 1)]
                                     (+ (* ni height)
                                        nj))]))
        remove-group (fn [neighbors]
                       (let [root (ffirst neighbors)
                             reachable (->> (iterate
                                             (fn [{seen :seen
                                                   [at & todo] :todo}]
                                               (if-not at
                                                 {::done seen}
                                                 (let [seen (conj seen at)]
                                                   {:seen seen
                                                    :todo (into todo
                                                                (remove seen)
                                                                (get neighbors at))})))
                                             {:seen #{}
                                              :todo [root]})
                                            (some ::done))]
                         (reduce dissoc
                                 neighbors
                                 reachable)))
        group-count (->> neighbors
                         (iterate remove-group)
                         (take-while seq)
                         count)]
    group-count))

(comment
  (day14-2 "flqrgnkx")

  (day14-2 "xlqgujun")
  

  )
