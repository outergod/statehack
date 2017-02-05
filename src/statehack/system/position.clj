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

(ns statehack.system.position
  "Positioning and coordinates"
  (:require [statehack.system.world :as world]
            [statehack.util :as util]
            [statehack.algebra :as algebra]
            [statehack.component :as c]))

(defn entities-at
  "Matching entities at given `floor` and `coords`"
  ([game floor coords]
   (let [entities (world/entities game)
         coords (set coords)]
     (filter #(and (= (::c/floor %) floor) (coords (::c/position %)))
       (vals entities))))
  ([game e]
   (entities-at game (::c/floor e) [(::c/position e)])))

(defn direct-neighbors
  "Neighbor entities at given `floor` around `coords`"
  [game coords floor]
  (entities-at game floor (algebra/neighbors coords)))

(defn entity-neighbors
  "Neighbor entities of `e`"
  [game e]
  (direct-neighbors game (::c/position e) (::c/floor e)))

(defn entity-delta
  "Calculate delta between `e1` and `e2`"
  [e1 e2]
  (util/matrix-subtract (::c/position e1) (::c/position e2)))
