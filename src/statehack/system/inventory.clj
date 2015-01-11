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
            [statehack.component :as c]))

(defn in-inventory? [e1 e2]
  {:pre [(:inventory e1) (:pickup e2)]}
  (some #{(:id e2)} (:inventory e1)))

(defn pick-up-item [game e1 e2]
  {:pre [(:inventory e1) (:pickup e2)]}
  (world/>> game [(:id e1) (:id e2)]
            #(world/update-entity-component %1 :inventory conj (:id %2))
            #(world/remove-entity-component %2 :position :floor)))

(defn drop-item [game e1 e2]
  {:pre [(in-inventory? e1 e2)]}
  (world/>> game [(:id e1) (:id e2)]
            #(world/update-entity-component %1 :inventory remove #{(:id %2)})
            #(world/add-entity-component %2 (c/position (:position %1)) (c/floor (:floor %1)))))

(defn available-pickups [game e]
  (let [{:keys [position floor]} e]
    (filter #(and (= position (:position e)) (= floor (:floor e)))
            (world/capable-entities game :pickup))))

#_(defn close [game e]
  (let [es (filter #(and (entity/capable? % :open) (:open %))
                   (world/entity-neighbors game e))]
    (case (count es)
      0 (messages/log game "No open door nearby.")
      1 (close-door game (first es))
      (defer/defer game es close-door))))
