(ns advent.day16
  (:require [clojure.spec.alpha :as s]
            [clojure.test :as test]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.rfs :as xrf]
            [net.cgrand.xforms.io :as xio]
            [criterium.core :as cc]))

(def example "s1,x3/4,pe/b")

(defn parse [strr-in]
  (into []
        (comp (mapcat (fn [str-in]
                        (str/split str-in #",")))
              (map (fn [line]
                     (case (first line)
                       \s {:do :spin
                           :length (Long/parseLong (subs line 1))}
                       \x (let [[from to] (str/split (subs line 1) #"/")]
                            {:do :exchange
                             :from (Long/parseLong from)
                             :to (Long/parseLong to)})
                       \p (let [[from to] (str/split (subs line 1) #"/")]
                            {:do :partner
                             :from (first from)
                             :to (first to)})
                       ))))
        (xio/lines-in strr-in)))

(defn apply-in [acc in]
  (case (:do in)
    :spin
    (let [[start end] (split-at (- (count acc) (:length in)) acc)]
      (-> []
          (into end)
          (into start)))

    :exchange
    (let [{:keys [from to]} in]
      (assoc acc
             to (get acc from)
             from (get acc to)))

    :partner
    (let [{:keys [from to]} in
          from-idx (.indexOf acc from)
          to-idx (.indexOf acc to)]
      (assoc acc
             from-idx to
             to-idx from))))

(defn day16 [ins]
  (reduce
    apply-in
    (mapv char (range (int \a)
                      (inc (int \p))))
    ins))

(defn day16-2 [ins]
  (let [start (mapv char (range (int \a)
                                (inc (int \p))))
        do-dance (fn [from]
                   (reduce apply-in from ins))
        period (->> start
                    (iterate do-dance)
                    rest
                    (take-while (complement #{start}))
                    (cons start))]
    (nth period (mod 1e9 (count period)))))

(comment
  (parse (java.io.StringReader. example))
  (parse (io/resource "day16.txt"))

  (time (day16 (parse (java.io.StringReader. example))))
  (time (->> (day16 (parse (io/resource "day16.txt")))
             (apply str)))

  (->> (day16-2 (parse (io/resource "day16.txt")))
       (apply str))
)
