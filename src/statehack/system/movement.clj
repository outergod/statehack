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

(ns statehack.system.movement
  (:require [statehack.entity :as entity]
            [statehack.system.messages :as messages]
            [statehack.system.world :as world]
            [statehack.system.unique :as unique]
            [statehack.system.input.receivers :as receivers]
            [statehack.system.obstacle :as obstacle]
            [statehack.system.levels :as levels]
            [statehack.system.door :as door]
            [statehack.util :as util]
            [statehack.algebra :as algebra]
            [clojure.set :as set]))

(def move-hierarchy
  "Hierarchy used for `available-moves`"
  (make-hierarchy))

(defn- available-moves-dispatch
  "Dispatcher for `available-moves`"
  [game e]
  (-> e :mobile :type))

(defmulti available-moves
  "Available moves determines where an entity can move to based on its `mobility`"
  #'available-moves-dispatch :hierarchy #'move-hierarchy)

(defmethod available-moves :default [& _] nil)

(defn move
  "Move entity `e` by `[x y]`"
  [game e [x y]]
  (world/update-entity-component game (:id e) :position util/matrix-add [x y]))

(defn relocate
  "Relocate entity `e` to `[x y]`"
  [game e [x y]]
  (world/update-entity-component game (:id e) :position (constantly [x y])))

(defn inbound-moves
  "Set of all possible inbound moves for `e`"
  [game e]
  (let [{:keys [foundation]} (levels/floor game (:floor e))]
    (set (filter #(levels/in-bounds? foundation (util/matrix-add (:position e) %))
                 algebra/neighbor-deltas))))

(defn- available-moves-common
  "Shared implementation of `available-moves`, using obstacles `es`"
  [game e es]
  (let [ds (set (map #(world/entity-delta % e) es))]
    (into {} (map (fn [pos] [pos #(move % e pos)])
                  (set/difference (inbound-moves game e) ds)))))

;;; Bipedal movement. Obstructed only by obstacle entities.
(defmethod available-moves :bipedal [game e]
  (let [es (obstacle/filter-obstacles (world/entity-neighbors game e))]
    (available-moves-common game e es)))

;;; Wheel movement. Can't move across doors.
(defmethod available-moves :wheels [game e]
  (let [es (world/entity-neighbors game e)
        os (set (obstacle/filter-obstacles (world/entity-neighbors game e)))
        doors (set (door/filter-doors es))]
    (available-moves-common game e (set/union os doors))))

(defn move-next
  "Move selector entity `sel` to next possible target"
  [game sel]
  (let [{:keys [targets]} (:mobile sel)
        targets (concat (rest targets) [(first targets)])
        e (world/entity game (first targets))]
    (world/update game [sel (:id sel) e (:id e)]
      (world/update-entity-component game (:id sel) :mobile assoc :targets targets)
      (relocate game sel (:position e)))))

(defn unavailable-moves
  "Mapping of unavailable movements to log messages"
  [game e]
  (let [os (obstacle/filter-obstacles (world/entity-neighbors game e))
        cs (set/difference algebra/neighbor-deltas (inbound-moves game e))]
    (merge (into {} (map (fn [o] [(world/entity-delta o e) #(messages/log % "There's an obstacle in the way.")]) os))
           (into {} (map (fn [c] [c #(messages/log % "Somehow, you can't move here...")]) cs)))))

(defn update-cursor
  "Update the cursor position based on the current input receiver"
  [game]
  (let [{:keys [entities receivers]} (world/state game)
        r (receivers/current game)
        e (unique/unique-entity game :cursor)
        [x y] (if (entity/capable? r :messages)
                [(+ (count (first (:messages r))) 2) 1]
                (:position r))]
    (world/update-entity-component game (:id e) :position (constantly [x y]))))
