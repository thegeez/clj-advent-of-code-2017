(ns advent.day10
  (:require [clojure.spec.alpha :as s]
            [clojure.test :as test]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.rfs :as xrf]
            [net.cgrand.xforms.io :as xio]
            [net.thegeez.advent.spec-parsing :as sp]
            [instaparse.core :as insta]))


(defn swap-indexes [start-idx length rope-count]
  (let [half-length (long (Math/ceil (/ length 2)))
        from-indexes (range start-idx
                            (inc (+ start-idx half-length)))
        to-indexes (range (dec (+ start-idx length))
                          (dec (- (+ start-idx length)
                                  half-length)) -1)
        wrap (fn [n] (mod n rope-count))]
    (zipmap (map wrap from-indexes)
            (map wrap to-indexes))))

(test/deftest swap-indexes-test
  (test/are [i l rc out] (= (swap-indexes i l rc) out)
    0 2 3 {0 1}
    0 3 5 {0 2, 1 1}
    3 4 5 {3 1, 4 0}
    1 5 5 {1 0, 2 4, 3 3}))

(defn swap [v [from to]]
  (let [f (get v from)
        t (get v to)]
    (assoc v
           from t
           to f)))

(defn step [{:keys [rope idx skip]} length]
  (let [next-rope (reduce swap
                          rope
                          (swap-indexes idx length (count rope)))]
    {:rope next-rope
     :idx (mod (+ idx length skip) (count rope))
     :skip (inc skip)}))

(defn day10 [lengths]
  (let [res (reduce step
                    {:rope (vec (range 0 256))
                     :idx 0
                     :skip 0}
                    lengths)
        [a b & xs] (:rope res)]
    (* a b)))

(comment
  (do (println "=======")
      (swap-indexes-test))

  (= (reductions
      step
      {:rope [0 1 2 3 4]
       :idx 0
       :skip 0}
      [3 4 1 5])
     [{:rope [0 1 2 3 4]
       :idx 0
       :skip 0}
      {:rope [2 1 0 3 4]
       :idx 3
       :skip 1}
      {:rope [4 3 0 1 2]
       :idx 3
       :skip 2}
      {:rope [4 3 0 1 2]
       :idx 1
       :skip 3}
      {:rope [3 4 2 1 0]
       :idx 4
       :skip 4}])

  (let [in (-> (io/resource "day10.txt")
               slurp
               (str/split #",")
               (->>
                (mapv #(Long/parseLong (str/trim %)))))]
    (day10 in))
  )

(def to-hex-char (zipmap (range 16)
                         "0123456789abcdef"))

(defn to-hex [nums]
  (transduce
   (comp (mapcat (fn [num]
                   (let [first8 (bit-shift-right num 4)
                         last8 (bit-and 0x0F num)]
                     [first8 last8])))
         (map to-hex-char))
   xrf/str
   nums))

(defn day10-2 [str-in]
  (let [to-ascii int
        ascii-str (->> (str/trim str-in)
                       (mapv to-ascii))
        lengths (into ascii-str
                      [17 31 73 47 23])
        sparse-hash (:rope (reduce step
                                   {:rope (vec (range 0 256))
                                    :idx 0
                                    :skip 0}
                                   (mapcat identity
                                           (repeat 64 lengths))))
        dense-hash (into []
                         (x/partition 16 16
                                      (x/reduce (fn
                                                  ([] 0x0)
                                                  ([acc] acc)
                                                  ([l r]
                                                   (bit-xor l r)))))
                         sparse-hash)
        knot-hash (to-hex dense-hash)]
    knot-hash))



(test/deftest tests2
  (test/are [in out] (= (day10-2 in) out)
    "" "a2582a3a0e66e6e86e3812dcb672a272"
    "AoC 2017" "33efeb34ea91902bb2f59c9920caa6cd"
    "1,2,3" "3efbe78a8d82f29979031a4aa0b16a9d"
    "1,2,4" "63960835bcdc130f0b66d7ff4f6a5a8e"))

(comment
  (tests2)

  (let [in (-> (io/resource "day10.txt")
               slurp
               str/trim)]
    (day10-2 in))
  )
