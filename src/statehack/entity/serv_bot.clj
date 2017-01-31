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

(ns statehack.entity.serv-bot
  (:require [statehack.entity :refer :all]
            [statehack.component :as c]))

(defn serv-bot [[x y z]]
  (entity
   (c/position [x y])
   (c/floor z)
   (c/alive true)
   (c/category :serv-bot)
   (c/mobile :wheels)
   (c/renderable :serv-bot)
   (c/obstacle)
   (c/vulnerable 20)
   (c/armor 20)
   (c/ai :serv-bot)
   (c/sight :sensors 5)
   #_(c/sight :omniscience nil)
   (c/memory)
   (c/skillset :melee (apply merge
                             (c/name "Appendages")
                             (c/weapon :melee 8 0 1 :appendages)))
   (c/inventory)))
