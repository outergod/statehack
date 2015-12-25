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

(ns statehack.system.slots
  (:require [statehack.system.world :as world]
            [statehack.entity :as entity]
            [clojure.set :as set]))

(defn available-slots [e]
  {:pre [(entity/capable? e :slots)]}
  (set (keys (:slots e))))

(defn slotted? [actor item]
  {:pre [(entity/capable? actor :slots)]}
  ((set/map-invert (:slots actor)) (:id item)))

(defn slot-item [game actor item slot]
  {:pre [((available-slots actor) slot)]}
  (world/update-entity-component game actor [:slots slot] (constantly (:id item))))

(defn unslot-item [game actor item]
  (if-let [slot (slotted? actor item)]
    (world/update-entity-component game actor [:slots slot] (constantly nil))
    game))

(defn slotted-items [actor]
  {:pre [(entity/capable? actor :slots)]}
  (set (filter identity (vals (:slots actor)))))


