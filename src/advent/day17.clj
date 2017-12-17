(ns advent.day17
  (:require [clojure.spec.alpha :as s]
            [clojure.test :as test]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.rfs :as xrf]
            [net.cgrand.xforms.io :as xio]
            [criterium.core :as cc]))


(defn spin [length {:keys [idx at v]}]
  (let [to (-> (mod (+ at length)
                    (count v))
               inc)
        idx (inc idx)]
    (when (zero? (mod idx 100000))
      (println idx))
    {:idx idx
     :at to
     :v (let [[pre post] (split-at to v)]
          (-> []
              (into pre)
              (conj idx)
              (into post)))}))

(defn day17 [length]
  (->> (iterate
        (partial spin length)
        {:idx 0
         :at 0
         :v [0]})
       (drop 2017)
       (first)
       :v
       (partition 2)
       (some (fn [[l r]]
               (when (= l 2017)
                 r)))))

(defn day17-2 [length]
  (let [final 50000000]
    (loop [at 0
           idx 0
           after nil]
      (when (zero? (mod idx 100000))
        (println idx))
      (let [idx (inc idx)]
        (if (= idx final)
          after
          (let [at (-> (mod (+ at length) idx)
                       inc)]
            (recur at
                   idx
                   (if (= 1 at)
                     idx
                     after))))))))

(comment
  (day17 3)
  (day17 367)
  (day17-2 3)
  (day17-2 367)
  
  )

