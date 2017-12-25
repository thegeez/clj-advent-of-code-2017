(ns advent.day25
  (:require [clojure.spec.alpha :as s]
            [clojure.test :as test]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.rfs :as xrf]
            [net.cgrand.xforms.io :as xio]
            [criterium.core :as cc]
            [instaparse.core :as insta]
            [clojure.walk :as walk]))

(def example
  "Begin in state A.
Perform a diagnostic checksum after 6 steps.

In state A:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state B.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state B.

In state B:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state A.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state A.
")


(def parser
  (insta/parser
   "instructions = 
startline + newline +
check-sum-steps-line + newline +
state-block+ +
<startline> = <'Begin in state '> + start-state + <'.'>
start-state = state
check-sum-steps-line = <'Perform a diagnostic checksum after '> + check-sum + <' steps.'>
state = #\"[A-Z]\"
check-sum = number
number = #\"[0-9]+\"
state-block = newline +
<'In state '> + state + <':'> + newline +
transition+
transition = <'  If the current value is '> + current-value + <':'> + newline +
<'    - Write the value '> + write-value + <'.'> + newline +
<'    - Move one slot to the '> + direction + <'.'> + newline +
<'    - Continue with state '> + continue-state + <'.'> + newline;
current-value = number
write-value = number
direction = right|left
right = <'right'>
left = <'left'>
continue-state = state
<newline> = <#\"\n\">
"
 ;;  :output-format :enlive
   ))

(defn parse [str-in]
  (->> (parser str-in)
       (walk/postwalk (fn [node]
                        (if (vector? node)
                          (let [[a b] node]
                            (case a
                              :state
                              (keyword b)
                              
                              :number
                              (Long/parseLong b)
                              
                              :right
                              :right
                              
                              :left
                              :left
                              
                              :transition
                              (let [[_tag [_current-value v] & rest] node]
                                {v (into {} rest)})

                              :state-block
                              (let [[_tag at & transitions] node]
                                {at (apply merge transitions)})

                              :instructions
                              (let [[_tag
                                     [_start-state start-state]
                                     [_cline [_sum check-sum]]
                                     & transitions] node]
                                {:start-state start-state
                                 :check-sum check-sum
                                 :transitions (apply merge transitions)})
                              
                              ;;else
                              node))
                          node)))))

(defn step [transitions world]
  (let [{:keys [at tape state]} world
        current-value (get tape at 0)
        trans (get-in transitions [state current-value])]
    (-> world
        (update :tape assoc at (:write-value trans))
        (update :at (get {:right inc
                          :left dec} (:direction trans)))
        (assoc :state (:continue-state trans)))))

(defn print-world [world]
  (let [{:keys [state at tape]} world
        tape (merge {at 0} tape)
        {:keys [min max]} (x/some
                           (comp (map key)
                                 (x/transjuxt {:min x/min
                                               :max x/max}))
                           tape)]
    (println "==========")
    (println "At: " at)
    (println "State: " state)
    (println "Tape: " (transduce
                       (mapcat (fn [idx]
                                 (if (= at idx)
                                   ["[" (get tape idx) "]"]
                                   [" " (get tape idx) " "])))
                       xrf/str
                       (range min (inc max))))
    world))

(defn check-sum [world]
  (transduce
   (keep (comp #{1} val))
   +
   (:tape world)))

(defn day25 [ins]
  (->> {:state (:start-state ins)
        :at 0
        :tape {0 0}}
       (iterate (partial step (:transitions ins)))
       ;;(map print-world)
       (drop (:check-sum ins))
       first
       print-world
       check-sum))

(comment
  (parse example)
  
  (parse (slurp (io/resource "day25.txt")))

  (day25 (parse example))
  (day25 (parse (slurp (io/resource "day25.txt"))))
  (print-world {:state :A, :at 0, :tape {0 0, 1 1, -1 1, -2 1}})
  
  )
