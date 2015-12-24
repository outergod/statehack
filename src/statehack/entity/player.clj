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

(ns statehack.entity.player
  (:require [statehack.entity :refer :all]
            [statehack.component :as c]))

(defn player [name [x y z] hp]
  (entity
   (c/unique :player)
   (c/alive true)
   (c/position [x y])
   (c/floor z)
   (c/name name)
   (c/category :human)
   (c/adaptive 0 0)
   (c/mobile :bipedal)
   (c/renderable :humanoid)
   (c/input :player)
   (c/obstacle)
   (c/vulnerable hp)
   #_(c/sight :omniscience nil)
   (c/sight :eyes 10)
   (c/memory)
   (c/inventory)
   (c/slots :melee nil :gun nil)))
