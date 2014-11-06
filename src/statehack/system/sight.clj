(ns statehack.system.sight
  "Visual calculation system"
  (:require [statehack.algebra :as algebra]
            [statehack.system.world :as world]
            [statehack.system.levels :as levels]
            [statehack.entity :as entity]
            [statehack.util :as util]))

(def opaque?-hierarchy "Hierarchy for `opaque?`" (make-hierarchy))

(defn derive-opaque?
  "Derive for `opaque?-hierarchy`"
  [tag parent]
  (alter-var-root #'opaque?-hierarchy derive tag parent))

(def opaque?-dispatch
  "Dispatch for `opaque?`"
  :opaque)

(defmulti opaque?
  "Is entity opaque?"
  {:arglists '([e])}
  #'opaque?-dispatch :hierarchy #'opaque?-hierarchy)

(defmethod opaque? :default [_] false)
(defmethod opaque? true [_] true)
(defmethod opaque? false [_] false)

(defmethod opaque? :door [e] (not (:open e)))

(defn filter-opaques [es]
  (filter opaque? es))

(defn visible-entities
  "Visible entities on `floor` within `mask`"
  [game floor mask]
  (entity/filter-capable [:renderable] (world/entities-at game floor mask)))

(defn visible-mask
  "Visible coordinates for sighted entity `e`."
  [game e]
  {:pre [(:sight e) (:position e) (:floor e)]}
  (let [[x y] (:position e)
        r (-> e :sight :distance)
        ps (set (map :position (filter-opaques (levels/on-floor (:floor e) (entity/filter-capable [:position :floor] (vals (world/entities game)))))))]
    (conj (set (mapcat (partial util/take-while-including (complement ps))
                       (algebra/visible-lines [x y] r)))
          [x y])))
