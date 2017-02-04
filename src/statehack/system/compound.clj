;;;; This file is part of statehack.
;;;;
;;;; Copyright © 2017 Alexander Kahl <ak@sodosopa.io>
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

(ns statehack.system.compound
  "System that handles compounds.

  Compounds are hierarchies of entities that allow synchronized behavior.
  This is especially important for entities that consist of multiple tiles."
  (:refer-clojure :exclude [parents])
  (:require [statehack.component :as c]
            [statehack.system.world :as world]))

(defn group
  "Group together `parent` and `children` by means of the `compound` component"
  [parent children]
  (cons (merge parent {::c/compound #::c{:parents nil :children (map :id children)}})
    (map #(merge % {::c/compound #::c{:parents [(:id parent)] :children nil}})
      children)))

(defn parents
  "Compound parents of entity"
  [game e]
  (map (world/entities game) (get-in e [::c/compound ::c/parents])))

(defn parent
  "Compound single parent of entity"
  [game e]
  (first (parents game e)))

(defn children
  "Compound children of entity"
  [game e]
  (map (world/entities game) (get-in e [::c/compound ::c/children])))
