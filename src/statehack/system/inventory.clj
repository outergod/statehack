;;;; This file is part of statehack.
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

(ns statehack.system.inventory
  (:require [statehack.system.world :as world]
            [statehack.component :as c]
            [statehack.entity :as entity]
            [statehack.system.input :as input]
            [statehack.system.input.receivers :as receivers]))

(defn in-inventory? [e1 e2]
  {:pre [(:inventory e1) (:pickup e2)]}
  (some #{(:id e2)} (:inventory e1)))

(defn pick-up-item [game e1 e2]
  {:pre [(:inventory e1) (:pickup e2)]}
  (world/>> game [(:id e1) (:id e2)]
            (fn [game actor item] (world/update-entity-component game actor :inventory conj (:id item)))
            (fn [game _ item] (world/remove-entity-component game item :position :floor))))

(defn drop-item [game e1 e2]
  {:pre [(in-inventory? e1 e2)]}
  (world/>> game [(:id e1) (:id e2)]
            #(world/update-entity-component %1 :inventory remove #{(:id %2)})
            #(world/add-entity-component %2 (c/position (:position %1)) (c/floor (:floor %1)))))

(defn available-pickups [game e]
  (entity/filter-capable [:pickup] (world/entities-at game e)))

(defn change-index [game e f]
  (let [player (->> [:inventory-menu :reference] (get-in e) (world/entity game))
        max (count (available-pickups game player))]
    (if (zero? max)
      game
      (world/update-entity-component game e [:inventory-menu :selected]
                                     (comp #(mod % max) f)))))

(defn pick-up [game {:keys [inventory-menu]}]
  (let [player (world/entity game (:reference inventory-menu))
        {:keys [selected]} inventory-menu]
    (if (empty? (available-pickups game player))
      game
      (pick-up-item game player (nth (available-pickups game player) selected)))))

(defn inventory-open? [game]
  (entity/capable? (receivers/current game) :inventory-menu))

(defmethod input/receive :inventory [game inventory input]
  (case (:key input)
    :escape (-> game receivers/pop-control (world/remove-entity inventory))
    \w (change-index game inventory inc)
    \x (change-index game inventory dec)
    (:enter \t) (pick-up game inventory)
    game))
