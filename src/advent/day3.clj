(ns advent.day3
  (:require [clojure.spec.alpha :as s]
            [clojure.test :as test]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]))

(defn step-side [{:keys [last-direction n x y]}]
  ;; n stays at 0,0, expand grid with a new side
  (case last-direction
    :down
    (let [go :right
          stride (+ 1 (* -1 x 2))
          next-x (+ x stride)
          next-n (+ n stride)]
      {:n next-n
       :last-direction go
       :x next-x
       :y y})
    :right
    (let [go :up
          stride (+ 1 (* y 2))
          next-y (- y stride)
          next-n (+ n stride)]
      {:n next-n
       :last-direction go
       :x x
       :y next-y})
    :up
    (let [go :left
          stride (* x 2)
          next-x (- x stride)
          next-n (+ n stride)]
      {:n next-n
       :last-direction go
       :x next-x
       :y y})
    :left
    (let [go :down
          stride (* -1 y 2)
          next-y (+ y stride)
          next-n (+ n stride)]
      {:n next-n
       :last-direction go
       :x x
       :y next-y}))
  )

(defn day3 [square-n]
  (->> (iterate step-side {:last-direction :down
                           :n 1
                           :x 0
                           :y 0})
       (some (fn [{:keys [n last-direction x y] :as state}]
               (when (<= square-n n)
                 ;; square n was found in the last side that was added to the grid
                 (let [diff (- n square-n)
                       [sx sy] (case last-direction
                                 :down [x (- y diff)]
                                 :left [(+ x diff) y]
                                 :up [x (+ y diff)]
                                 :right [(- x diff) y])
                       manhattan-distance (+ (Math/abs sx)
                                             (Math/abs sy))]
                   manhattan-distance)))))
  )


(test/deftest tests
  (test/are [in out] (= (day3 in) out)
    1 0
    12 3
    23 2
    1024 31
    ))

(comment
  (day3 265149)
  ;;438

  (tests)
  )


(defn iter-coords []
  (iterate
   (fn [{:keys [x y dx dy n]}]
     (let [n (inc n)
           x (+ x dx)
           y (+ y dy)
           [dx dy] (cond
                     ;; from right turn up
                     (and (<= 0 x)
                          (<= 0 y)
                          (= (- x 1) y))
                     [0 -1]
                     ;; from up turn left
                     (and (<= 0 x)
                          (< y 0)
                          (= x (* -1 y)))
                     [-1 0]
                     ;; from left turn down
                     (and (< x 0)
                          (< y 0)
                          (= x y))
                     [0 1]
                     ;; from down turn right
                     (and (< x 0)
                          (<= 0 y)
                          (= x (* -1 y)))
                     [1 0]
                     :else
                     [dx dy])]
       {:n n
        :x x
        :y y
        :dx dx
        :dy dy}))
   {:x 0 :y 0 :dx 1 :dy 0 :n 1}))


(defn grid-vals []
  (let [coords (iter-coords)]
    (reductions
     (fn [acc coord]
       (let [{:keys [x y]} coord
             sum (->> (for [dx [-1 0 1]
                            dy [-1 0 1]]
                        (get (:seen acc) [(+ x dx) (+ y dy)] 0))
                      (reduce +))]
         (-> acc
             (update :seen assoc [x y] sum)
             (assoc :last sum))))
     {:seen {[0 0] 1}
      :last 0}
     coords)))


(defn day3-2 [in]
  (some
   (fn [{:keys [last prev-last]}]
     (when (< in last)
       last))
   (grid-vals)))

(test/deftest tests2
  (test/is (= (->> (grid-vals)
                   (map :last)
                   rest
                   (take 23))
              (sort [147  142  133  122   59
                     304    5    4    2   57
                     330   10    1    1   54
                     351   11   23   25   26
                     362  747  806])))
  (test/are [in out] (= (day3-2 in) out)
    140 142
    325 330
    800 806))

(comment
  (tests2)

  (take 10 (iter-coords))
  (->> (take 10 (grid-vals))
       (map :last))

  (day3-2 265149)
  ;; 266330
  )
