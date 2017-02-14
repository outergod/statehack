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

(ns statehack.entity.room
  "Room-related entities"
  (:require [statehack.entity :refer [conform uuid]]
            [statehack.component :as c]
            [clojure.spec :as s]))

(defn floor
  "Floor entity"
  [color]
  (conform #::c{:id (uuid) :renderable :floor :color color}))

(defn wall
  "Wall entity"
  [color]
  (conform #::c{:id (uuid) :renderable :wall :color color :room true :obstacle :always :opaque :always}))

(defn solid
  "Solid entity"
  []
  (conform #::c{:id (uuid) :renderable #::c{:tile :nihil :color 0} :obstacle :always :opaque :always}))

(defn door
  "Door entity"
  [type open?]
  (conform
    #::c{:id (uuid) :renderable :door :obstacle :door :opaque :door :room true
         :door #::c{:type type :open? open?}}))
  
(defn blast-door
  "Blast door entity"
  [open?]
  (conform #::c{:id (uuid) :door #::c{:type :blast :open? open?}}))
