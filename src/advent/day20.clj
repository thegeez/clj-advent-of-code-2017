(ns advent.day20
  (:require [clojure.spec.alpha :as s]
            [clojure.test :as test]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.rfs :as xrf]
            [net.cgrand.xforms.io :as xio]
            [criterium.core :as cc]))

(def example
  "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>
p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>")

;; p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
;; p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>                         (0)(1)

;; p=< 4,0,0>, v=< 1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
;; p=< 2,0,0>, v=<-2,0,0>, a=<-2,0,0>                      (1)   (0)

;; p=< 4,0,0>, v=< 0,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
;; p=<-2,0,0>, v=<-4,0,0>, a=<-2,0,0>          (1)               (0)

;; p=< 3,0,0>, v=<-1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
;; p=<-8,0,0>, v=<-6,0,0>, a=<-2,0,0>                         (0)  

(defn parse-3d [s]
  (let [in-brackets (subs s 3 (dec (count s)))]
    (->> (str/split in-brackets #",")
         (mapv #(Long/parseLong %)))))

(defn zero-dist [p]
  (reduce + (map #(Math/abs %) p)))

(defn parse [in]
  (into []
        (map-indexed (fn [idx line]
                       (let [[p v a] (str/split line #", ")
                             p (parse-3d p)
                             dist (zero-dist p)]
                         {:id idx
                          :p p
                          :v (parse-3d v)
                          :a (parse-3d a)
                          :dist dist})))
        (xio/lines-in in)))

(defn day20 [config]
  (->> config
       (map (fn [p]
              (assoc p
                     :adist (zero-dist (:a p))
                     :vdist (zero-dist (:v p))
                     :pdist (zero-dist (:p p)))))
       (sort-by (juxt :adist :vdist :pdist))
       (take 5)))

(defn step [points]
  (->> (into []
             (map (fn [{:keys [p v a] :as point}]
                    (let [v' (mapv + v a)
                          p' (mapv + p v')
                          dist (zero-dist p')]
                      (assoc point :v v' :p p' :dist dist))))
             points)
       (into []
             (comp (x/by-key :p
                             (x/into []))
                   (x/for [[k [v & vs]] %
                           :when (nil? vs)]
                     v)))))



(defn day20-2 [config]
  (->> config
       (iterate step)
       (x/some
        (comp (x/partition 100 1
                           (comp (map count)
                                 (x/into [])))
              (keep (fn [[c & counts]]
                      (when (and (every? (fn [cc]
                                           (= cc c))
                                         counts)
                                 (not= c 1000))
                        c)))))))

(comment
  (parse-3d "p=<-2781,1254,-451>")
  (parse (io/resource "day20.txt"))


  (day20 (parse (java.io.StringReader. example)))
  (day20 (parse (io/resource "day20.txt")))

  (day20-2 (parse (java.io.StringReader. example)))
  (day20-2 (parse (io/resource "day20.txt")))
  (day20-2 [{:id 0
             :p [-2 0 0]
             :v [1 0 0]
             :a [0 0 0]}
            {:id 1
             :p [2 0 0]
             :v [-1 0 0]
             :a [0 0 0]}
            {:id 2
             :p [1 2 3]
             :v [1 2 3]
             :a [1 2 3]}])


  )
