(ns advent.day19
  (:require [clojure.spec.alpha :as s]
            [clojure.test :as test]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.rfs :as xrf]
            [net.cgrand.xforms.io :as xio]
            [criterium.core :as cc]))


(def example
"     |          
     |  +--+    
     A  |  C    
 F---|----E|--+ 
     |  |  |  D 
     +B-+  +--+") 


(defn to-coords [in]
  (into {}
        (comp (map-indexed (fn [row-idx line]
                             (map-indexed (fn [col-idx s]
                                            [[row-idx col-idx] s])
                                          line)))
              cat
              (remove (fn [[[r c] s]]
                        (= s \space))))
        (xio/lines-in in)))

(defn day19 [coords]
  (let [[[r c] s :as start] (some (fn [c]
                                    (find coords [0 c])) (range))
        down [1 0]
        walked (->> (iterate
                     (fn [{[r c] :at, dir :dir, prev-dir :prev-dir, steps :steps, seen :seen :as all}]
                       (let [[[nr nc] new-dir] (case dir
                                                 :down (let [[at nd] (or (find coords [(+ r 1) c])
                                                                         (find coords [(+ r 2) c]))]
                                                         (if (= nd \+)
                                                           [at :cross]
                                                           [at :down]))
                                                 :up (let [[at nd] (or (find coords [(- r 1) c])
                                                                       (find coords [(- r 2) c]))]
                                                       (if (= nd \+)
                                                         [at :cross]
                                                         [at :up]))
                                                 :left (let [[at nd] (or (find coords [r (- c 1)])
                                                                         (find coords [r (- c 2)]))]
                                                         (if (= nd \+)
                                                           [at :cross]
                                                           [at :left]))
                                                 :right (let [[at nd] (or (find coords [r (+ c 1)])
                                                                          (find coords [r (+ c 2)]))]
                                                          (if (= nd \+)
                                                            [at :cross]
                                                            [at :right]))
                                                 :cross (some (fn [[[dr dc] new-dir]]
                                                                (when-let [[at nd] (find coords [(+ r dr) (+ c dc)])]
                                                                  (if (= nd \+)
                                                                    [at :cross]
                                                                    [at new-dir])))
                                                              (remove (fn [[_ new-dir]]
                                                                        (= prev-dir
                                                                           (get {:up :down
                                                                                 :down :up
                                                                                 :left :right
                                                                                 :right :left} new-dir)))
                                                                      [[[-1  0] :up]
                                                                       [[ 1  0] :down]
                                                                       [[ 0 -1] :left]
                                                                       [[ 0  1] :right]])))
                             sym (get coords [r c])
                             char-found (when (not (get (set "|+-") sym))
                                          sym)
                             seen (if char-found
                                    (conj seen char-found)
                                    seen)]
                         (if (and nr nc)
                           {:at [nr nc]
                            :dir new-dir
                            :prev-dir (if (= new-dir :cross)
                                        prev-dir
                                        new-dir)
                            :steps (inc steps)
                            :seen seen}
                           {::answer [(apply str seen)
                                      steps]})))

                     {:at [r c]
                      :dir :down
                      :prev-dir :down
                      :steps 1
                      :seen []})
                    (some ::answer))]
    walked))

(comment
  (day19 (to-coords (java.io.StringReader. example)))
  (day19 (to-coords (io/resource "day19.txt")))

  )
