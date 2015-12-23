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
            [statehack.system.input.receivers :as receivers]
            [statehack.entity.menu.inventory :as inventory-menu]))

(def default-frame {:pickup :floor
                    :inventory :inventory})

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
            (fn [game actor item] (world/update-entity-component game actor :inventory (partial remove #{(:id item)})))
            (fn [game actor item] (world/add-entity-component game item (c/position (:position actor)) (c/floor (:floor actor))))))

(defn available-pickups [game e]
  (entity/filter-capable [:pickup] (world/entities-at game e)))

(defn- frame-items [game e frame]
  (if (= frame :floor)
    (available-pickups game e)
    (map (partial world/entity game) (:inventory e))))

(defn change-index [game {:keys [inventory-menu] :as menu} f]
  (let [{:keys [reference index frame]} inventory-menu
        player (world/entity game reference)
        max (count (frame-items game player frame))]
    (world/update-entity-component game menu [:inventory-menu :index]
                                   (comp (if (zero? max) (constantly 0) #(mod % max)) f))))

(defn change-frame [game {:keys [inventory-menu] :as menu} frame]
  (if (= (inventory-menu :type) :pickup)
    (world/>> game [(:id menu)]
              #(world/update-entity-component %1 %2 [:inventory-menu :frame] (constantly frame))
              #(change-index %1 %2 identity))
    game))

(defn pick-up-or-drop [game {:keys [inventory-menu] :as menu}]
  (let [{:keys [reference index frame type]} inventory-menu
        actor (world/entity game reference)
        items (frame-items game actor frame)]
    (if (or (= type :inventory) (empty? items))
      game
      (let [f (if (= frame :floor) pick-up-item drop-item)]
        (-> game (f actor (nth items index)) (change-index menu identity))))))

(defn inventory-open? [game]
  (entity/capable? (receivers/current game) :inventory-menu))

(defn inventory-type [game]
  (get-in (receivers/current game) [:inventory-menu :type]))

(defn open [game player type]
  (let [i (inventory-menu/inventory (:id player) type (default-frame type))]
    (-> game (world/add-entity i) (receivers/push-control i))))

(defmethod input/receive :inventory-menu [game menu input]
  (case (:key input)
    :escape (-> game receivers/pop-control (world/remove-entity menu))
    \w (change-index game menu inc)
    \x (change-index game menu dec)
    \a (change-frame game menu :floor)
    \d (change-frame game menu :inventory)
    :enter (pick-up-or-drop game menu)
    game))
