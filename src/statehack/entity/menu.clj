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

(ns statehack.entity.menu
  (:require [statehack.entity :refer [conform uuid]]
            [statehack.component :as c]
            [statehack.component.menu :as cm]
            [clojure.spec :as s]))

(defn inventory
  "Menu inventory entity"
  [id type frame]
  (conform
    #::c {:id (uuid) :input :inventory-menu
          ::cm/inventory-menu #::cm{::inventory-type type ::inventory-reference id
                                    ::inventory-index 0 ::inventory-frame frame}}))
