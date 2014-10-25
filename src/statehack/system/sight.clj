(ns statehack.system.sight
  "Visual calculation system"
  (:require [statehack.algebra :as algebra]
            [statehack.system.obstacle :as obstacle]
            [statehack.system.world :as world]
            [statehack.entity :as entity]
            [statehack.util :as util]))

(defn visible-mask
  "Visible coordinates for sighted entity `e`."
  [game e]
  {:pre [(:sight e)]}
  (let [[x y] (:position e)
        r (-> e :sight :distance)
        ps (set (map :position (obstacle/filter-obstacles (entity/filter-capable [:position] (vals (world/entities game))))))]
    (conj (set (mapcat (partial util/take-while-including (complement ps))
                       (algebra/visible-lines [x y] r)))
          [x y])))
