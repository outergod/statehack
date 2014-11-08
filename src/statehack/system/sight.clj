(ns statehack.system.sight
  "Visual calculation system"
  (:require [statehack.algebra :as algebra]
            [statehack.system.world :as world]
            [statehack.system.levels :as levels]
            [statehack.entity :as entity]
            [statehack.system.levels :as levels]
            [statehack.util :as util]))

;;; opaque? multimethod

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

(defn filter-opaques
  "Filter opaques from `es`"
  [es]
  (filter opaque? es))

;;; visible-mask multimethod

(def visible-mask-hierarchy "Hierarchy for `visible-mask`" (make-hierarchy))

(defn derive-visible-mask
  "Derive for `visible-mask-hierarchy`"
  [tag parent]
  (alter-var-root #'visible-mask-hierarchy derive tag parent))

(defn visible-mask-dispatch
  "Dispatch for `visible-mask`"
  [game e]
  (-> e :sight :type))

(defmulti visible-mask
  "Is entity visible-mask"
  {:arglists '([game e])}
  #'visible-mask-dispatch :hierarchy #'visible-mask-hierarchy)

(defn- visible-mask-common
  "Shared implementation of `visible-mask` for organic and sensoric sighted entities"
  [game e]
  (let [[x y] (:position e)
        r (-> e :sight :distance)
        ps (set (map :position (filter-opaques (levels/on-floor (:floor e) (entity/filter-capable [:position :floor] (vals (world/entities game)))))))]
    (conj (set (mapcat (partial util/take-while-including (complement ps))
                       (algebra/visible-lines [x y] r)))
          [x y])))

(defmethod visible-mask :eyes [game e]
  (visible-mask-common game e))

(defmethod visible-mask :sensors [game e]
  (visible-mask-common game e))

(defmethod visible-mask :omniscience [game e]
  (let [{:keys [foundation]} (levels/entity-floor game e)
        [w h] foundation]
    (set (for [x (range w) y (range h)] [x y]))))

(defn visible-entities
  "Visible entities on `floor` within `mask`"
  ([game floor mask]
     (entity/filter-capable [:renderable] (world/entities-at game floor mask)))
  ([game e]
     (let [{:keys [floor]} (levels/entity-floor game e)
           mask (visible-mask game e)]
       (visible-entities game floor mask))))
