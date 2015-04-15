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

(ns statehack.system.viewport
  (:require [statehack.system.render :as render]
            [statehack.system.world :as world]
            [statehack.system.levels :as levels]
            [statehack.util :as util]))

(defn update-viewport [game e f]
  (update-in game [:viewport] f))

(defn center-viewport [game e]
  (update-viewport game e (constantly (e :position))))

(defn snap-back
  "Snap the viewport back to the visible area of the screen"
  [{:keys [viewport layout] :as game}]
  (println layout)
  game)
