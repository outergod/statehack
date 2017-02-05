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

(ns statehack.system.viewport
  (:require [statehack.component :as c]
            [statehack.system.layout :as layout]
            [statehack.system.levels :as levels]
            [statehack.util :as util]))

(defn snap
  "Snap back `[x y]` into the visible area

  Given a foundation of size `[w h]` and visible area of `[sw sh]`."
  [[x y] [sw sh] [w h]]
  (let [[a b] [(int (/ sw 2)) (int (/ sh 2))]
        [x0 y0] [(- x a) (- y b)]
        x0 (min (max x0 0) (- w sw))
        y0 (min (max y0 0) (- h sh))]
    (util/matrix-add [x0 y0] [a b])))

(defn update-viewport [game e f]
  (let [{:keys [layout]} game
        dimensions (get-in (layout/by-id layout) [:world-view :dimensions])
        {:keys [::c/foundation]} (levels/entity-floor game e)]
    (update-in game [:viewport] #(snap (f %) dimensions foundation))))

(defn center-on [game e]
  (update-viewport game e (constantly (::c/position e))))
