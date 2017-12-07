(ns advent.day7
  (:require [clojure.spec.alpha :as s]
            [clojure.test :as test]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.io :as xio]
            [clojure.walk :as walk]))


(def example "pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)")

(defn parse-line [line]
  (let [[word weight _arrow & children] (str/split line #" ")
        weight (Long/parseLong (subs weight 1 (dec (count weight))))
        children (mapv (fn [^String s]
                         (if (.endsWith s ",")
                           (subs s 0 (dec (count s)))
                           s))
                       children)]
    {:id word
     :weight weight
     :children children}))

(defn parse [lines]
  (map (fn [line]
         (parse-line line))
       lines))

(defn find-root [config]
  (->> (reduce
        (fn [parents {:keys [id children]}]
          (reduce
           (fn [parents child-id]
             (assoc parents child-id id))
           (merge {id nil}
                  parents)
           children))
        {} ;; id -> maybe parent-id
        config)
       (some (fn [[child parent]]
               (when-not parent
                 child)))))

(comment
  (find-root (parse (str/split-lines example)))

  (find-root (parse (str/split-lines (slurp (io/resource "day7.txt")))))

  
  )


(defn fix-weight [config]
  (let [root (find-root config)
        nodes (zipmap (map :id config)
                      config)
        add-branches (fn add-branches [root]
                       (let [node (get nodes root)
                             children (mapv
                                       add-branches
                                       (:children node))]
                         {:id (:id node)
                          :weight (:weight node)
                          :sum-weight (+ (:weight node)
                                         (reduce + (map :sum-weight children)))
                          :children children}))
        tree (add-branches root)]
    (walk/postwalk
     (fn [node]
       (if (map? node)
         (if-let [answer (some (fn [c]
                                 (when (:answer c)
                                   c))
                               (:children node))]
           answer
           (if-let [children (seq (:children node))]
             (let [children-weight (map :sum-weight children)]
               (if (apply = children-weight)
                 node
                 (let [[[w1 g1] [w2 g2]] (seq (group-by :sum-weight children))
                       ;; either g1 or g2 will be the single askew node
                       [wrong-node needed-weight] (if (= (count g1) 1)
                                                    [(first g1)
                                                     (:sum-weight (first g2))]
                                                    [(first g2)
                                                     (:sum-weight (first g1))])
                       fixed-weight (+ (:weight wrong-node)
                                       (- needed-weight
                                          (:sum-weight wrong-node)))]
                   {:answer fixed-weight})))
             node))
         node))
     tree)
    ))

(comment
  (fix-weight (parse (str/split-lines example)))

  (fix-weight (parse (str/split-lines (slurp (io/resource "day7.txt")))))
  )
