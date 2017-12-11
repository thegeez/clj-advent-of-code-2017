(ns advent.day11
  (:require [clojure.spec.alpha :as s]
            [clojure.test :as test]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.rfs :as xrf]
            [net.cgrand.xforms.io :as xio]
            [criterium.core :as cc]
            [clojure.walk :as walk]
            [clojure.core.reducers :as r]))
;;   \ n  /
;; nw +--+ ne
;;   /    \
;; -+      +-
;;   \    /
;; sw +--+ se
;;   / s  \

(def dirs [:n :ne :se :s :sw :nw])

(def replacement-moves
  (let [north-moves (merge-with merge
                                ;; going direction and then its neighbor in dirs has no shortcut
                                ;; i.e. :n,:ne = :n,:ne
                                ;; - {}

                                ;; going direction and direction + 2 = direction + 1
                                ;; i.e. :n,:se = :ne
                                (let [opp 2]
                                  (zipmap dirs
                                          (map hash-map
                                               (take 6 (drop opp (cycle dirs)))
                                               (take 6 (drop (dec opp) (cycle dirs))))))
                                ;; 2 away neighbourd that cancel each other
                                ;; i.e. : :n,:s = :cancelled
                                (let [opp 3]
                                  (zipmap dirs
                                          (map hash-map
                                               (take 6 (drop opp (cycle dirs)))
                                               (repeat :cancelled))))
                                ;; 3 away neighbour
                                ;; i.e :n,:sw = :nw
                                (let [opp 4]
                                  (zipmap dirs
                                          (map hash-map
                                               (take 6 (drop opp (cycle dirs)))
                                               (take 6 (drop (inc opp) (cycle dirs))))))
                                ;; 4 away neighbour
                                ;; i.e :n,:nw = :n,:nw
                                ;; {}
                                ;; 5 away neighbour
                                ;; i.e :n, :n = :n, :n
                                )
        projections (for [i (range 1 6)]
                      (zipmap (take 6 dirs)
                              (take 6 (drop i (cycle dirs)))))
        all-moves (->> projections
                       (map (fn [projection]
                              (walk/postwalk-replace projection north-moves)))
                       (reduce
                        merge
                        north-moves))]
    all-moves))


(defn compact [idx-moves]
  (let [out (reduce
             (fn [idx-moves [i j]]
               (let [im (get idx-moves i)
                     jm (get idx-moves j)
                     replace-move (-> (get replacement-moves im)
                                      (get jm))]
                 (condp = replace-move
                   :cancelled
                   (dissoc idx-moves i j)

                   nil
                   idx-moves

                   ;;else
                   (-> idx-moves
                       (assoc i replace-move)
                       (dissoc j))
                   )))
             idx-moves
             ;; 'first' in idx-moves paired with all others
             (for [i (keys idx-moves)
                   j (rest (keys idx-moves))]
               [i j])
             )]
    (if (identical? out idx-moves)
      (reduced idx-moves)
      out)))

(defn steps-away [idx-moves]
  (->> (iterate compact idx-moves)
       (some (fn [res]
               (when (reduced? res) ;; fixpoint for idx-moves
                 (count (unreduced res)))))))

(defn day11 [str-in]
  (let [moves (-> str-in
                  (str/trim)
                  (str/split #",")
                  (->> (map keyword)))
        ;; shrinking vector as idx to content map
        idx-moves (zipmap (range) moves)]
    (steps-away idx-moves)))

(test/deftest tests
  (test/are [in out] (= (day11 in) out)
    "ne,ne,ne" 3 ;; steps away.
    "ne,ne,sw,sw" 0 ;;steps away (back where you started).
    "ne,ne,s,s" 2 ;;steps away (se,se).
    "se,sw,se,sw,sw" 3 ;; steps away (s,s,sw)
    "s,s,se,sw,se,s,sw,sw" 6
    ))

(comment
  (compact (zipmap (range) [:n ;;:n :s
                            ]))
  (do (println "-----------")
      (tests))
  (day11 (slurp (io/resource "day11.txt")))

)

;;https://www.redblobgames.com/grids/hexagons/#distances
(def steps
  ;; [x y z]
  {:n [1 -1 0]
   :ne [0 -1 1]
   :se [-1 0 1]
   :s [-1 1 0]
   :sw [0 1 -1]
   :nw [1 0 -1]})

(defn distance [[x y z]]
  (/ (+ (Math/abs x)
        (Math/abs y)
        (Math/abs z))
     2))

(defn day11-2 [str-in]
  (let [moves (-> str-in
                  (str/trim)
                  (str/split #",")
                  (->> (mapv keyword)))
        distances (reductions
                   (fn [dist move]
                     (mapv + dist (get steps move)))
                   [0 0 0]
                   moves)]
    (->> distances
         (map distance)
         (apply max))))

(comment
  (day11-2 (slurp (io/resource "day11.txt")))
  )


;; alternate

(defn day11-alt [str-in]
  (let [moves (-> str-in
                  (str/trim)
                  (str/split #",")
                  (->> (map keyword)))
        idx-moves (mapv vector (range) moves)

        shortest-paths (reductions
                        (fn recur-reduce [acc [idx dir]]
                          (let [[j replace-move] (some
                                                  (fn [[j jm]]
                                                    (when-let [replace-move (-> (get replacement-moves jm)
                                                                                (get dir))]
                                                      [j replace-move]))
                                                  (reverse acc))]
                            (if j
                              (if (= replace-move :cancelled)
                                (dissoc acc j)
                                ;; this move might compact with an existing move
                                (recur-reduce (dissoc acc j) [idx replace-move]))
                              (assoc acc idx dir))))
                        (conj {} (first idx-moves))
                        (rest idx-moves))]
    (->> shortest-paths
         (map count)
         ((fn [res]
            {:part1 (last res)
             :part2 (apply max res)})))))

(comment
  (day11-alt (slurp (io/resource "day11.txt")))

  )
