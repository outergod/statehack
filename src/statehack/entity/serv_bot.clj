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
  (:require [statehack.entity :refer [uuid]]
            [statehack.component :as c]))

(defn serv-bot
  "Serv-Bot entity"
  []
  #::c{:id (uuid)
       :name "Serv-Bot"
       :category :serv-bot
       :renderable :serv-bot
       :alive true
       :obstacle true
       :memory {}
       :inventory []
       :ai :serv-bot
       :armor 20
       :mobile #::c{:type :wheels}
       :vulnerable #::c{:hp 20 :max 20}
       :sight #::c{:type :sensors :distance 5}
       :skillset {:melee #::c{:name "Appendages"
                              :weapon #::c{:type :melee :transition :appendages
                                           :damage 8 :penetration 0 :offense 1}}}})
