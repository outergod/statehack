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
  (:require [statehack.entity :refer :all]
            [statehack.component :as c]))

(defn wall
  "Wall entity"
  [color]
  (entity
   (c/renderable :wall)
   (c/color color)
   (c/room)
   (c/obstacle)
   (c/opaque)))

(defn solid
  "Solid entity"
  []
  (entity
   (c/renderable {:tile :nihil :color 0})
   (c/obstacle)
   (c/opaque)))

(defn door
  "Door entity"
  [type open?]
  (entity
   (c/renderable :door)
   (c/door type open?)
   (c/room)
   (c/obstacle :door)
   (c/opaque :door)))

(defn blast-door
  "Blast door entity"
  [open?]
  (entity (c/door :blast open?)))
