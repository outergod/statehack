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

(ns statehack.system.name
  (:refer-clojure :exclude [name])
  (:require [statehack.entity :as entity]))

(def category-names
  {:human "Human"
   :serv-bot "Serv-Bot"})

(defn name [e]
  (cond (entity/capable? e :name) (:name e)
        (entity/capable? e :category) (category-names (:category e))
        :default "Unknown"))
