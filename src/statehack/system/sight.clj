;;;; This file is part of statehack.
;;;;
;;;; Copyright © 2014-2017 Alexander Kahl <ak@sodosopa.io>
;;;;
;;;; statehack is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; statehack is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with statehack.  If not, see <http://www.gnu.org/licenses/>.

(ns statehack.system.sight
  "statehack visual calculation system"
  (:require [statehack.algebra :as algebra]
            [statehack.component :as c]
            [statehack.entity :as entity]
            [statehack.system.door :as door]
            [statehack.system.levels :as levels]
            [statehack.system.position :as pos]
            [statehack.system.world :as world]
            [statehack.util :as util]))

;;; opaque? multimethod

(def opaque?-hierarchy "Hierarchy for `opaque?`" (make-hierarchy))

(defn derive-opaque?
  "Derive for `opaque?-hierarchy`"
  [tag parent]
  (alter-var-root #'opaque?-hierarchy derive tag parent))

(def opaque?-dispatch
  "Dispatch for `opaque?`"
  ::c/opaque)

(defmulti opaque?
  "Is entity opaque?"
  {:arglists '([e])}
  #'opaque?-dispatch :hierarchy #'opaque?-hierarchy)

(defmethod opaque? :default [_] true)
(defmethod opaque? nil [_] false)
(defmethod opaque? :door [e] (not (door/open? e)))

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
  (-> e ::c/sight ::c/type))

(defmulti visible-mask
  "Is entity visible-mask"
  {:arglists '([game e])}
  #'visible-mask-dispatch :hierarchy #'visible-mask-hierarchy)

(defn- visible-mask-common
  "Shared implementation of `visible-mask` for organic and sensoric sighted entities"
  [game e]
  (let [[x y] (::c/position e)
        r (-> e ::c/sight ::c/distance)
        ps (set (map ::c/position (filter-opaques (levels/on-level (::c/level e) (entity/filter-capable [::c/position ::c/level] (vals (world/entities game)))))))]
    (conj (set (mapcat (partial util/take-while-including (complement ps))
                       (algebra/visible-lines [x y] r)))
          [x y])))

(defmethod visible-mask :eyes [game e]
  (visible-mask-common game e))

(defmethod visible-mask :sensors [game e]
  (visible-mask-common game e))

(defmethod visible-mask :omniscience [game e]
  (let [{:keys [::c/foundation]} (levels/entity-level game e)
        [w h] foundation]
    (set (for [x (range w) y (range h)] [x y]))))

(defn visible-entities
  "Visible entities on `level` within `mask`"
  ([game level mask]
   (entity/filter-capable [::c/renderable] (pos/entities-at game level mask)))
  ([game e]
   (let [{:keys [::c/level]} (levels/entity-level game e)
         mask (visible-mask game e)]
     (visible-entities game level mask))))
