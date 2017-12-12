(ns advent.day12
  (:require [clojure.spec.alpha :as s]
            [clojure.test :as test]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.rfs :as xrf]
            [net.cgrand.xforms.io :as xio]
            [criterium.core :as cc]
            [datascript.core :as d]))


(def example
  "0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5")

(defn day12 [str-in]
  (let [pipes (->> str-in
                   str/split-lines
                   (mapv (fn [line]
                           (let [[from _arrow & tos] (str/split line #"\s")
                                 tos (mapv #(first (str/split % #",")) tos)
                                 ]
                             {:from (Long/parseLong from)
                              :tos (mapv #(Long/parseLong %) tos)}))))

        db (-> (d/empty-db {:node-id { :db/unique :db.unique/identity }
                            :reaches { :db/cardinality :db.cardinality/many }})
               (d/db-with (mapcat
                           (fn [temp-id {:keys [from tos]}] pipes
                             (into [[:db/add temp-id :node-id from]]
                                   (for [to tos]
                                     [:db/add temp-id :reaches to])))
                           (map #(* -1 %) (range))
                           pipes)))
        rules '[[(reachable ?e1 ?e2)
                 [?e1 :reaches ?e2]]
                [(reachable ?e1 ?e2)
                 [?e1 :_reaches ?e2]]
                [(reachable ?e1 ?e2)
                 [?e1 :reaches ?t]
                 (reachable ?t ?e2)] ]
        
        res (d/q '{:find [(pull ?en [:node-id])]
                   :in [$ %]
                   :where [(reachable [:node-id 0] ?en)]}
                 db
                 rules)]
    (count res)))

(comment
  (day12 example)

  (day12 (slurp (io/resource "day12.txt")))
  )


(defn day12-2 [str-in]
  (let [pipes (->> str-in
                   str/split-lines
                   (mapv (fn [line]
                           (let [[from _arrow & tos] (str/split line #"\s")
                                 tos (mapv #(first (str/split % #",")) tos)]
                             {:from (Long/parseLong from)
                              :tos (mapv #(Long/parseLong %) tos)}))))

        db (-> (d/empty-db {:node-id { :db/unique :db.unique/identity }
                            :reaches { :db/cardinality :db.cardinality/many }})
               (d/db-with (mapcat
                           (fn [temp-id {:keys [from tos]}] pipes
                             (into [[:db/add temp-id :node-id from]]
                                   (for [to tos]
                                     [:db/add temp-id :reaches to])))
                           (map #(* -1 %) (range))
                           pipes)))
        rules '[[(reachable ?e1 ?e2)
                 [?e1 :reaches ?e2]]
                [(reachable ?e1 ?e2)
                 [?e1 :_reaches ?e2]]
                [(reachable ?e1 ?e2)
                 [?e1 :reaches ?t]
                 (reachable ?t ?e2)] ]

        remove-group (fn [db]
                       (let [root-node-id (or (:node-id (d/entity db [:node-id 0]))
                                              (ffirst (d/q '{:find [?ni]
                                                             :where [[?e :node-id ?ni]]}
                                                           db)))
                             in-group (d/q '{:find [(pull ?en [:db/id :node-id])]
                                             :in [$ % ?root-node-id]
                                             :where [[?er :node-id ?root-node-id]
                                                     (reachable ?er ?en)]}
                                           db
                                           rules
                                           root-node-id)]
                         (d/db-with db
                                    (for [[node-e] in-group]
                                      [:db.fn/retractEntity (:db/id node-e)]))))]
    (->> (iterate remove-group db)
         (take-while (fn [db]
                       (seq (d/q '{:find [?e]
                                   :where [[?e]]}
                                 db))))
         count)))

(comment
  (day12-2 example)

  (day12-2 (slurp (io/resource "day12.txt")))
  )
