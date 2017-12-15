(ns advent.day14
  (:require [clojure.spec.alpha :as s]
            [clojure.test :as test]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.rfs :as xrf]
            [net.cgrand.xforms.io :as xio]
            [criterium.core :as cc]))

;; A sample start 65, while generator B uses 8921

(def example-pairs
  [[1092455   430625591]
   [1181022009  1233683848]
   [245556042  1431495498] ;; => match
   [1744312007   137874439]
   [1352636452   285222916]])

(defn day15 [a b]
  (loop [a (long a)
         b (long b)
         out 0
         i 0]
    (if (= i 40000000)
      out
      (let [a (-> (* a 16807)
                  (mod 2147483647))
            b (-> (* b 48271)
                  (mod 2147483647))]
        (recur a
               b
               (if (== (bit-and 0xFFFF a)
                       (bit-and 0xFFFF b))
                 (inc out)
                 out)
               (inc i))))))

(defn day15-2 [a b]
  (loop [a (long a)
         b (long b)
         out 0
         i 0]
    (if (= i 5000000)
      out
      (let [a (loop [a a]
                (let [a (-> (* a 16807)
                            (mod 2147483647))]
                  (if (== 0 (rem a 4))
                    a
                    (recur a))))
            b (loop [b b]
                (let [b (-> (* b 48271)
                            (mod 2147483647))]
                  (if (== 0 (rem b 8))
                    b
                    (recur b))))]
        (recur a
               b
               (if (== (bit-and 0xFFFF a)
                       (bit-and 0xFFFF b))
                 (inc out)
                 out)
               (inc i))))))

(comment
  (day15 65 8921)
  (day15 512 191)

  (day15-2 65 8921)
  (day15-2 512 191)
  )

