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

(ns statehack.system.door
  (:require [statehack.entity :as entity]
            [statehack.system.messages :as messages]
            [statehack.system.defer :as defer]
            [statehack.system.input.receivers :as receivers]
            [statehack.system.world :as world]
            [statehack.system.time :as time]
            [statehack.system.transition :as transition]
            [statehack.system.compound :as compound]
            [clojure.set :as set]))

(defn open?
  "Is the door open?"
  [door]
  (get-in door [:door :open]))

(defn- operate-door
  "Common code for opening/closing doors"
  [game door state]
  (world/update-entity-component game (:id door) [:door :open] (constantly state)))

(def door-hierarchy
  "Hierarchy of door types"
  (make-hierarchy))

(defn derive-door
  "Derive for `door-hierarchy`"
  [tag parent]
  (alter-var-root #'door-hierarchy derive tag parent))

(defn- door-dispatch
  "Dispatch for door multimethods`"
  [game door]
  (get-in door [:door :type]))

(defmulti door-sound "Door sound transition" #'door-dispatch
  :hierarchy #'door-hierarchy)

(defmethod door-sound :default [game _]
  (transition/transition game (transition/sound :door)))

(defmethod door-sound :blast [game _]
  (transition/transition game (transition/sound :blast-door)))

(defmulti open-door "Open a door" #'door-dispatch
  :hierarchy #'door-hierarchy)

(defn- open-close-door-default-common
  "Common open/close default code"
  [game door state]
  (world/update game [door (:id door)]
    (operate-door game door state)
    (door-sound game door)))

(defn- open-close-door-compound-common
  "Common open/close compound code"
  [game door state]
  (let [parent (compound/parent game door)]
    (world/update game [parent (:id parent)
                        doors (conj (map :id (compound/children game parent)) (:id parent))]
      (reduce #(operate-door %1 %2 state) game doors)
      (door-sound game parent))))

(defmethod open-door :default [game door]
  (open-close-door-default-common game door true))

(defmethod open-door :compound [game door]
  (open-close-door-compound-common game door true))

(defmulti close-door "Close a door" #'door-dispatch
  :hierarchy #'door-hierarchy)

(defmethod close-door :default [game door]
  (open-close-door-default-common game door false))

(defmethod close-door :compound [game door]
  (open-close-door-compound-common game door false))

(defn- available-open-close-common
  "Available open/close common code"
  [game e f]
  (let [es (filter #(and (entity/capable? % :door) (f %))
             (world/entity-neighbors game e))]
    (into {} (map (fn [door] [(world/entity-delta door e) #(open-door % door)]) es))))

(defn available-open [game e]
  (available-open-close-common game e (complement open?)))

(defn available-close [game e]
  (available-open-close-common game e open?))

(defn close [game e]
  (let [es (filter #(and (entity/capable? % :door) (open? %))
             (world/entity-neighbors game e))]
    (case (count es)
      0 (messages/log game "No open door nearby.")
      1 (close-door game (first es))
      (defer/defer game es close-door))))

(defn filter-doors [es]
  (entity/filter-capable [:door] es))

;;; Hierarchy

;; Blast doors are compound doors
(derive-door :blast :compound)
