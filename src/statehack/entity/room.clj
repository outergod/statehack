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

(ns statehack.entity.room
  (:require [statehack.entity :refer :all]
            [statehack.component :as c]))

(defn wall [[x y z] color]
  (entity
   (c/renderable :wall)
   (c/color color)
   (c/position [x y])
   (c/floor z)
   (c/room)
   (c/obstacle)
   (c/opaque)))

(defn solid [[x y z]]
  (entity
   (c/renderable {:tile :nihil :color 0})
   (c/position [x y])
   (c/floor z)
   (c/obstacle)
   (c/opaque)))

(defn door [[x y z] open?]
  (entity
   (c/renderable :door)
   (c/position [x y])
   (c/floor z)
   (c/door open?)
   (c/room)
   (c/obstacle :door)
   (c/opaque :door)))
