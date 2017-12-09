(ns advent.day9
  (:require [clojure.spec.alpha :as s]
            [clojure.test :as test]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.rfs :as xrf]
            [net.cgrand.xforms.io :as xio]
            [net.thegeez.advent.spec-parsing :as sp]))


(defn day9 [in]
  (loop [[c & [_ & skip-one :as cs]] in
         in-garbage false
         group-level 0
         sum 0]
    (if-not c
      sum
      
      (cond
        (= c \!) ;; ignore ! and next char
        (recur skip-one
               in-garbage
               group-level
               sum)

        (not (contains? #{\{ \} \< \>} c)) ;; ignore regular char
        (recur cs
               in-garbage
               group-level
               sum)

        :else
        (if in-garbage
          (if (= c \>)
            (recur cs
                   false
                   group-level
                   sum)
            (recur cs
                   in-garbage
                   group-level
                   sum))
          ;; not in garbage
          (cond
            (= c \<)
            (recur cs
                   true
                   group-level
                   sum)
          
            (= c \{) ;; open group
            (recur cs
                   in-garbage
                   (inc group-level)
                   sum)

            (= c \})
            (recur cs
                   in-garbage
                   (dec group-level)
                   (+ sum group-level))))))))


(test/deftest tests
  (test/are [in out] (= (day9 in) out)
    "{}" 1 ;;, score of 1.
    "{{{}}}" 6 ;;, score of 1 + 2 + 3 = 6.
    "{{},{}}" 5 ;;, score of 1 + 2 + 2 = 5.
    "{{{},{},{{}}}}" 16 ;; , score of 1 + 2 + 3 + 3 + 3 + 4 = 16.
    "{<a>,<a>,<a>,<a>}" 1 ;;, score of 1.
    "{{<ab>},{<ab>},{<ab>},{<ab>}}" 9 ;;, score of 1 + 2 + 2 + 2 + 2 = 9.
    "{{<!!>},{<!!>},{<!!>},{<!!>}}" 9 ;;, score of 1 + 2 + 2 + 2 + 2 = 9.
    "{{<a!>},{<a!>},{<a!>},{<ab>}}" 3 ;;, score of 1 + 2 = 3.

    ))

(comment
  (tests)

  (day9 (slurp (io/resource "day9.txt")))

  )

(defn day9-2 [in]
  (loop [[c & [_ & skip-one :as cs]] in
         in-garbage false
         sum 0]
    (if-not c
      sum
      
      (cond
        (= c \!) ;; ignore ! and next char
        (recur skip-one
               in-garbage
               sum)

        (and in-garbage
             (= c \>))
        (recur cs
               false
               sum)

        (and (not in-garbage)
             (= c \<))
        (recur cs
               true
               sum)

        :else
        (recur cs
               in-garbage
               (if in-garbage
                 (inc sum)
                 sum))))))


(test/deftest tests2
  (test/are [in out] (= (day9-2 in) out)
    "<>" 0
    "<random characters>" 17
    "<<<<>" 3
    "<{!>}>" 2
    "<!!>" 0
    "<!!!>>" 0
    "<{o\"i!a,<{i<a>" 10
    ))

(comment
  (tests2)

  (day9-2 (slurp (io/resource "day9.txt")))
  )




