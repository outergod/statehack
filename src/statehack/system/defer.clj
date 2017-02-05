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

(ns statehack.system.defer
  (:require [statehack.component :as c]
            [statehack.entity :as entity]
            [statehack.system.input.receivers :as receivers]
            [statehack.system.world :as world]))

(defn defer [game es action]
  (let [sel (entity/selector (::c/position (first es)) action (map ::c/id es))]
    (world/update game [e (::c/id sel)]
      (world/add-entity game sel)
      (receivers/push-control game e))))

(defn fulfill [game e]
  (let [{:keys [::c/deferred ::c/selector]} e
        es (:entities (world/state game))
        t (es (first selector))]
    (world/update game []
      (world/remove-entity game (::c/id e))
      (receivers/pop-control game)
      (deferred game t))))

(defn abort [game e]
  (world/update game []
    (world/remove-entity game (::c/id e))
    (receivers/pop-control game)))
