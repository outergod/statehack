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

(ns statehack.system.status
  (:require [clojure.pprint :refer [cl-format]]
            [statehack.entity :as entity]
            [statehack.system.name :as name]
            [statehack.system.world :as world]
            [statehack.system.unique :as unique]
            [statehack.system.messages :as messages]))

(defn player-status [e]
  (let [{:keys [adaptive hp]} e
        {:keys [current max]} hp
        hp-order (inc (int (Math/log10 max)))]
    (cl-format nil (str "~a | HP: ~" hp-order "d/~d | XP: ~d | Level: ~d") (name/name e) current max (:xp adaptive) (:level adaptive))))

(defn text [game e]
  (let [p (unique/unique-entity game :player)]
    (if (entity/capable? e :messages)
      (messages/current e)
      (player-status p))))
