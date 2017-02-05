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

(ns statehack.system.slots
  "statehack slots system"
  (:require [clojure.set :as set]
            [statehack.component :as c]
            [statehack.entity :as entity]
            [statehack.system.world :as world]))

(defn available-slots [e]
  {:pre [(entity/capable? e ::c/slots)]}
  (set (keys (::c/slots e))))

(defn slotted? [actor item]
  {:pre [(entity/capable? actor ::c/slots)]}
  ((set/map-invert (::c/slots actor)) (::c/id item)))

(defn- operate-slot
  "slot/unslot common code"
  [game actor slot x]
  (world/update-in-entity-component game (::c/id actor) ::c/slots [slot] (constantly x)))

(defn slot [game actor item slot]
  {:pre [((available-slots actor) slot)]}
  (operate-slot game actor slot (::c/id item)))

(defn unslot [game actor item]
  (if-let [slot (slotted? actor item)]
    (operate-slot game actor slot nil)
    game))

(defn slotted-items [actor]
  {:pre [(entity/capable? actor ::c/slots)]}
  (set (filter identity (vals (::c/slots actor)))))

(defn slot-item [game actor slot]
  {:pre [(entity/capable? actor ::c/slots)]}
  (world/entity game (get-in actor [::c/slots slot])))
