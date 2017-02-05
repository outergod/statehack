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

(ns statehack.system.status
  (:require [clojure.pprint :refer [cl-format]]
            [statehack.component :as c]
            [statehack.entity :as entity]
            [statehack.system.messages :as messages]
            [statehack.system.name :as name]
            [statehack.system.unique :as unique]))

(defn player-status [e]
  (let [{:keys [::c/adaptive ::c/vulnerable]} e
        {:keys [::c/hp ::c/max]} vulnerable
        hp-order (inc (int (Math/log10 max)))]
    (cl-format nil (str "~a | HP: ~" hp-order "d/~d | XP: ~d | Level: ~d") (name/name e) current max (::c/xp adaptive) (::c/level adaptive))))

(defn text [game e]
  (let [p (unique/unique-entity game :player)]
    (if (entity/capable? e ::c/messages)
      (messages/current e)
      (player-status p))))
