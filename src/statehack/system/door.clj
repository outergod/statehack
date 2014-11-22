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

(ns statehack.system.door
  (:require [statehack.entity :as entity]
            [statehack.entity.selector :as selector]
            [statehack.system.messages :as messages]
            [statehack.system.defer :as defer]
            [statehack.system.input.receivers :as receivers]
            [statehack.system.world :as world]
            [statehack.system.time :as time]
            [statehack.system.transition :as transition]
            [clojure.set :as set]))

(defn door-sound [game]
  (transition/transition game (transition/sound :door)))

(defn open-door [game e]
  (-> game
      (world/update-entity-component e :open (constantly true))
      door-sound))

(defn available-open [game e]
  (let [es (filter #(and (entity/capable? % :open)
                         (not (:open %)))
                   (world/entity-neighbors game e))]
    (into {} (map (fn [door] [(world/entity-delta door e) #(open-door % door)]) es))))

(defn close-door [game e]
  (-> game (world/update-entity-component e :open (constantly false)) (transition/transition (transition/sound :door)) time/pass-time))

(defn available-close [game e]
  (let [es (filter #(and (entity/capable? % :open)
                         (:open %))
                   (world/entity-neighbors game e))]
    (into {} (map (fn [door] [(world/entity-delta door e) #(close-door % door)]) es))))

(defn close [game e]
  (let [es (filter #(and (entity/capable? % :open) (:open %))
                   (world/entity-neighbors game e))]
    (case (count es)
      0 (messages/log game "No open door nearby.")
      1 (close-door game (first es))
      (defer/defer game es close-door))))

(defn filter-doors [es]
  (entity/filter-capable [:open] es))
