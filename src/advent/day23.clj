(ns advent.day23
  (:require [clojure.spec.alpha :as s]
            [clojure.test :as test]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.rfs :as xrf]
            [net.cgrand.xforms.io :as xio]
            [criterium.core :as cc]))


(defn parse [in]
  (into []
        (map (fn [line]
               (let [[ins arg0 arg1] (str/split line #"\s")
                     num-or-reg (fn [in]
                                  (try
                                    (Long/parseLong in)
                                    (catch Exception _
                                      (keyword in))))]
                 {:op (keyword ins)
                  :arg0 (num-or-reg arg0)
                  :arg1 (num-or-reg arg1)})))
        (xio/lines-in in)))

(defn get-or-num [regs arg]
  (if (number? arg)
    arg
    (get regs arg)))

(defn execute [prog state]
  (let [{:keys [regs pointer mul-count]} state]
    (if-let [in (get prog pointer)]
      (let [{:keys [op arg0 arg1]} in]
        (case op
          :set
          (assoc state
                 :regs (assoc regs arg0 (get-or-num regs arg1))
                 :pointer (inc pointer))

          :sub
          (assoc state
                 :regs (update regs arg0 - (get-or-num regs arg1))
                 :pointer (inc pointer))

          :mul
          (assoc state
                 :regs (update regs arg0 * (get-or-num regs arg1))
                 :pointer (inc pointer)
                 :mul-count (inc mul-count))

          :jnz
          (let [x (get-or-num regs arg0)]
            (if-not (zero? x)
              (assoc state
                     :pointer (+ pointer (get-or-num regs arg1)))
              (assoc state
                     :pointer (inc pointer))))

          :optimize18
          (let [b (get regs :b)
                d (get regs :d)
                f (if (zero? (mod b d))
                    0
                    (get regs :f))]
            (assoc state
                   :regs (assoc regs
                                :f f
                                :g 0
                                :e b)
                   :pointer 20))

          :optimize10
          (let [b (get regs :b)
                d (get regs :d)
                f (if (some (fn [d]
                              (zero? (mod b d)))
                            (range 2 b))
                    0
                    (get regs :f))]
            (assoc state
                   :regs (assoc regs
                                :f f
                                :g 0
                                :e b
                                :d b)
                   :pointer 24))
          ))
      ;; pointer outside prog range
      {::answer mul-count})))

(defn day23 [prog]
  (->> {:regs (zipmap [:a :b :c :d :e :f :g :h]
                      (repeat 0))
        :pointer 0
        :mul-count 0}
       (iterate (partial execute prog))
       (some ::answer)))

(defn find-cycle [min-length max-length]
  (let [count-run-length (fn [n]
                           (comp (x/partition n n)
                                 (x/partition 2 1)
                                 (drop-while (fn [[l r]]
                                               (not= l r)))
                                 (take-while (fn [[l r]]
                                               (= l r)))
                                 (x/transjuxt {:run (comp x/last
                                                          (map first))
                                               :repeats x/count})
                                 (x/into {})
                                 ))]
    (comp (map :pointer)
          (x/transjuxt (into {}
                             (map (fn [n]
                                    [n (count-run-length n)]))
                             (range min-length max-length))))))

(def noop {:op :jnz
           :arg0 0
           :arg1 0} )

(defn day23-2 [prog]
  (x/some
   (comp
    (map-indexed (fn [idx state]
                   (assoc state :run idx)))
    (take 1000000)
    #_ (comp (x/by-key :pointer
                      x/count)
             (x/into {}))
    ;; check if state after 20 holds
    #_(comp (keep (fn [state]
                    (when (= (:pointer state) 20)
                      (assoc state :correct (and (zero? (get-in state [:regs :g]))
                                                 (= (get-in state [:regs :e])
                                                    (get-in state [:regs :b])))))))
            (partition-by :correct)
            (map (fn [p]
                   {:correct (first p)
                    :count (count p)}))
            (x/into []))

    ;; check if state after 24 holds
    #_(comp (keep (fn [state]
                    (when (= (:pointer state) 24)
                      (assoc state :correct (and (zero? (get-in state [:regs :g]))
                                                 (= (get-in state [:regs :e])
                                                    (get-in state [:regs :b]))
                                                 (= (get-in state [:regs :d])
                                                    (get-in state [:regs :b])))))))
            (partition-by :correct)
            (map (fn [p]
                   {:correct (first p)
                    :count (count p)}))
            (x/into []))
    #_(find-cycle 1 30)

    (halt-when (fn [state]
                 (or #_(= (:pointer state) 20)
                     (not (get prog (:pointer state))))))
    x/last
    )
   (->> {:regs (assoc (zipmap [:b :c :d :e :f :g :h]
                              (repeat 0))
                      :a 1)
         :pointer 0
         :mul-count 0}
        (iterate (partial execute prog)))))


(comment
  (parse (io/resource "day23.txt"))
  (day23 (parse (io/resource "day23.txt")))
  (def p (parse (io/resource "day23.txt")))
  (def p (assoc p
                18 {:op :optimize18}
                11 noop, 12 noop, 13 noop, 14 noop, 15 noop, 16 noop, 17 noop, 19 noop
                10 {:op :optimize10}
                20 noop, 21 noop, 22 noop, 23 noop
                ))
  (day23-2 p)

  ;; cycle two
  {13 {:run [14 15 16 17 18 20 21 22 23 10 11 12 13], :repeats 76921},
   26 {:run [14 15 16 17 18 20 21 22 23 10 11 12 13 14 15 16 17 18 20 21 22 23 10 11 12 13], :repeats 38459}}

  (mapv (juxt identity #(get p %))
        (range 10 24))

  [[10 {:op :set, :arg0 :e, :arg1 2}]
   [11 {:op :jnz, :arg0 0, :arg1 0}]
   [12 {:op :jnz, :arg0 0, :arg1 0}]
   [13 {:op :jnz, :arg0 0, :arg1 0}]
   [14 {:op :jnz, :arg0 0, :arg1 0}]
   [15 {:op :jnz, :arg0 0, :arg1 0}]
   [16 {:op :jnz, :arg0 0, :arg1 0}]
   [17 {:op :jnz, :arg0 0, :arg1 0}]
   [18 {:op :optimize18}]
   [19 {:op :jnz, :arg0 :g, :arg1 -8}]
   [20 {:op :sub, :arg0 :d, :arg1 -1}]
   [21 {:op :set, :arg0 :g, :arg1 :d}]
   [22 {:op :sub, :arg0 :g, :arg1 :b}]
   [23 {:op :jnz, :arg0 :g, :arg1 -13}]]

  ;; 9:  d = 2
  ;; 10: do { e = 2
  ;; 11-19; if (zero? (mod b d)) f = 0
  ;; 20:    d++
  ;; 21:    g = d
  ;; 22:    g -= b
  ;; 23: while (g != 0)

  ;; f = 0 if (zero? (mod b d)) for d until b

  )





(comment

  ;; find-cycle ONE
  [{24 [[18 19 11 12 13 14 16 17
         18 19 11 12 13 14 16 17
         18 19 11 12 13 14 16 17] 414],
    16 [[18 19 11 12 13 14 16 17
         18 19 11 12 13 14 16 17] 623],
    8 [[18 19 11 12 13 14 16 17] 1247]}]

  (mapv (juxt identity #(get p %))
        [18 19 11 12 13 14 16 17])

  (mapv (juxt identity #(get p %))
        [11 12 13 14 15 16 17 18 19])
  [[11 {:op :set, :arg0 :g, :arg1 :d}]
   [12 {:op :mul, :arg0 :g, :arg1 :e}]
   [13 {:op :sub, :arg0 :g, :arg1 :b}]
   [14 {:op :jnz, :arg0 :g, :arg1 2}]
   [15 {:op :set, :arg0 :f, :arg1 0}]
   [16 {:op :sub, :arg0 :e, :arg1 -1}]
   [17 {:op :set, :arg0 :g, :arg1 :e}]
   [18 {:op :sub, :arg0 :g, :arg1 :b}]
   [19 {:op :jnz, :arg0 :g, :arg1 -8}]]
  ;; 8: f = 1
  ;; 9: d = 2
  ;; 10: e = 2
  ;; do
  ;;   g = d
  ;;   g *= e
  ;;   g -= b
  ;;   if (g == 0) { f = 0
  ;;              }
  ;;   e++
  ;;   g = e
  ;;   g -= b
  ;; while (g != 0)
  

  ;; -> write g, e & f
  ;; -> after f maybe zero, g == zero, e == b
  ;; -> from e to (b + 1)  do d*e - b
  ;; any e to b+1 ever d*e = b
  ;; f == zero when (zero? (mod b d))

  )


(defn gcd [l r]
  (some (fn [gcd]
          (println "gcd" gcd "l" (zero? (mod l gcd)))
          (when (and (zero? (mod l gcd))
                     (zero? (mod r gcd)))
            gcd))
        (do (println "l" l "r" r)
            (range (min l r) 0 -1))))
(comment


  (range 9 0 -1)
  (gcd 9 6)
  
  (gcd 107900 124900)
  

  107900, :c 124900
  )
