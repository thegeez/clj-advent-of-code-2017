(ns advent.day8
  (:require [clojure.spec.alpha :as s]
            [clojure.test :as test]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.rfs :as xrf]
            [net.cgrand.xforms.io :as xio]))

(def example "b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10")

(defn parse [in-lines]
  (mapv (fn [line]
          (let [[register inc-dec arg _if read-register op rhs] (str/split line #" ")]
            {:register (symbol register)
             :inc-dec (symbol inc-dec)
             :arg (Long/parseLong arg)
             :read-register (symbol read-register)
             :op (symbol op)
             :rhs (Long/parseLong rhs)}))
        in-lines))

(defn guard [regs in]
  (let [{:keys [read-register op rhs]} in
        lhs (get regs read-register 0)
        cop (get {'> >
                  '< <
                  '<= <=
                  '>= >=
                  '== ==
                  '!= not=} op)]
    (cop lhs rhs)
    ))

(defn execute [regs in]
  (if (guard regs in)
    (let [register (:register in)
          plus-minus (get {'inc +
                           'dec -} (:inc-dec in))
          arg (:arg in)]
      (update regs register (fnil plus-minus 0) arg))
    regs))

(defn day8 [ins]
  (let [regs (reduce
              execute
              {}
              ins)]
    (apply max (vals regs))
    ))

(defn day8-2 [ins]
  (let [all-regs (reductions
                  execute
                  {}
                  ins)]
    (apply max (mapcat vals all-regs))
    ))

(comment
  (day8 (parse (str/split-lines example)))

  (day8 (parse (str/split-lines (slurp (io/resource "day8.txt")))))

  (day8-2 (parse (str/split-lines example)))

  (day8-2 (parse (str/split-lines (slurp (io/resource "day8.txt")))))

  )
