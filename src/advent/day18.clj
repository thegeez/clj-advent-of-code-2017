(ns advent.day18
  (:require [clojure.spec.alpha :as s]
            [clojure.test :as test]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.rfs :as xrf]
            [net.cgrand.xforms.io :as xio]
            [criterium.core :as cc]))

(def example
  "set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2")

(def example-two "snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d")

(defn parse [code]
  (into []
        (map (fn [line]
               (let [[ins arg0 arg1] (str/split line #" ")
                     register (first arg0)
                     maybe-register-or-int (when arg1
                                             (if-let [n (try
                                                          (Long/parseLong arg1)
                                                          (catch Exception _
                                                            nil))]
                                               {:num n}
                                               {:arg-register (first arg1)}))]
                 (merge {:op (keyword ins)
                         :register register}
                        maybe-register-or-int))))
        (xio/lines-in code)))

(defn get-argument [regs in]
  (if-let [n (:num in)]
    n
    (get regs (:arg-register in) 0)))

(defn execute [ins]
  (fn [{:keys [pointer regs] :as exe}]
    (let [{:keys [op register] :as in} (get ins pointer)]
      (if-not in
        {::answer :terminated}
        (case (:op in)
          :set
          (-> exe
              (assoc-in [:regs register] (get-argument regs in))
              (update :pointer inc))
          :add
          (-> exe
              (update-in [:regs register] (fnil + 0) (get-argument regs in))
              (update :pointer inc))
          :mul
          (-> exe
              (update-in [:regs register] (fnil * 0) (get-argument regs in))
              (update :pointer inc))
          :mod
          (-> exe
              (update-in [:regs register] (fnil mod 0) (get-argument regs in))
              (update :pointer inc))
          :snd
          (-> exe
              (assoc-in [:regs :sound] (get regs register))
              (update :pointer inc))
          :rcv
          (if (zero? (get regs register 0))
            (update exe :pointer inc)
            (if-let [sound (get regs :sound)]
              {::answer sound}
              (update exe :pointer inc)))
          :jgz
          (if (< 0 (get regs register 0))
            (update exe :pointer + (get-argument regs in))
            (update exe :pointer inc))
          )))))

(defn day18 [ins]
  (->> (iterate
        (execute ins)
        {:pointer 0
         :regs {}})
       (some ::answer)))


(defn execute-thread [exe ins pid send-to-pid]
  (let [{:keys [pointer regs]} (get exe pid)
        {:keys [op register] :as in} (get ins pointer)]
    (if-not in
      (assoc-in exe [pid :terminated] true)
      (case (:op in)
        :set
        (-> exe
            (assoc-in [pid :regs register] (get-argument regs in))
            (update-in [pid :pointer] inc))
        :add
        (-> exe
            (update-in [pid :regs register] (fnil + 0) (get-argument regs in))
            (update-in [pid :pointer] inc))
        :mul
        (-> exe
            (update-in [pid :regs register] (fnil * 0) (get-argument regs in))
            (update-in [pid :pointer] inc))
        :mod
        (-> exe
            (update-in [pid :regs register] (fnil mod 0) (get-argument regs in))
            (update-in [pid :pointer] inc))
        :jgz
        (if (< 0 (if (contains? (set "0123456789") register)
                   (Long/parseLong (str register))
                   (get regs register 0)))
          (update-in exe [pid :pointer] + (get-argument regs in))
          (update-in exe [pid :pointer] inc))

        :snd
        (-> exe
            (update-in [send-to-pid :chan] conj (or (get regs register)
                                                    (Long/parseLong (str register))))
            (update-in [pid :send-count] inc)
            (update-in [pid :pointer] inc))
        :rcv
        (if-let [v (peek (get-in exe [pid :chan]))]
          (-> exe
              (assoc-in [pid :regs register] v)
              (update-in [pid :pointer] inc)
              (assoc-in [pid :waiting] false)
              (update-in [pid :chan] pop))
          (-> exe
              (assoc-in [pid :waiting] true)))
        ))))

(defn execute-par [ins]
  (fn [{pzero 0
        pone 1
        :as exe}]
    (if (and (or (get pzero :waiting)
                 (get pzero :terminated))
             (or (get pone :waiting)
                 (get pone :terminated)))
      {::answer {:why {0 (select-keys pzero [:waiting :terminated])
                       1 (select-keys pzero [:waiting :terminated])}
                 :pone-send-count (get pone :send-count)}}
      (-> exe
          (execute-thread ins 0 1)
          (execute-thread ins 1 0)))))

(defn day18-2 [ins]
  (->> (iterate
        (execute-par ins)
        {0 {:pointer 0
            :send-count 0
            :chan clojure.lang.PersistentQueue/EMPTY
            :waiting false
            :regs {\p 0}}
         1 {:pointer 0
            :send-count 0
            :chan clojure.lang.PersistentQueue/EMPTY
            :waiting false
            :regs {\p 1}}})
       (some ::answer)))



(comment
  (day18 (parse (java.io.StringReader. example)))
  (day18 (parse (io/resource "day18.txt")))

  (day18-2 (parse (java.io.StringReader. example-two)))
  (day18-2 (parse (io/resource "day18.txt")))
  )
