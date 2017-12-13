(ns advent.day13
  (:require [clojure.spec.alpha :as s]
            [clojure.test :as test]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.rfs :as xrf]
            [net.cgrand.xforms.io :as xio]
            [criterium.core :as cc]))


(def example
  "0: 3
1: 2
4: 4
6: 4")

(defn parse [str-in]
  (into []
        (map (fn [line]
               (let [[d r] (->> (str/split line #": ")
                                (map #(Long/parseLong %)))]
                 {:depth d
                  :range r})))
        (xio/lines-in str-in)))


(comment
  (parse (java.io.StringReader. example))
  (parse (io/resource "day13.txt"))
  )

(defn day13 [config]
  (transduce
   (keep (fn [{:keys [depth range] :as layer}]
           (let [episode (- (* 2 range) 2)
                 within-episode (mod depth episode)
                 scanner-at-depth (if (< depth within-episode)
                                    ;; moving up
                                    (- range (- episode range))
                                    ;; moving down
                                    within-episode)]
             (when (zero? scanner-at-depth)
               (* depth range)))))
   +
   config))


(comment
  (day13 (parse (java.io.StringReader. example)))
  (day13 (parse (io/resource "day13.txt")))
  )

(defn day13-2 [config]
  (some (fn [wait]
          (when-not (some (fn [{:keys [depth range] :as layer}]
                            (let [at (+ depth wait)
                                  episode (- (* 2 range) 2)
                                  within-episode (mod at episode)
                                  scanner-at-depth (if (< depth within-episode)
                                                     ;; moving up
                                                     (- range (- episode range))
                                                     ;; moving down
                                                     within-episode)]
                              (when (zero? scanner-at-depth)
                                :hit)))
                          config)
            wait))
        (range)))



(comment
  (day13-2 (parse (java.io.StringReader. example)))
  (day13-2 (parse (io/resource "day13.txt")))

  )
