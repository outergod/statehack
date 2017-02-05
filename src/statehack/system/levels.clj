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

(ns statehack.system.levels
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [statehack.component :as c]
            [statehack.entity :as entity]
            [statehack.entity.container :as container]
            [statehack.entity.room :as room]
            [statehack.entity.weapon :as weapon]
            [statehack.system.compound :as compound]
            [statehack.system.world :as world]
            [statehack.util :as util]))

(defn label
  "Add label component to entity"
  [e label]
  (entity/conform (merge e {::c/label label})))

(defn position
  "Position entity at floor and coordinates"
  [e [x y] floor]
  (entity/conform (merge e #::c{:position [x y] :floor floor})))

(def common-tiles
  {\X #(room/wall :lightblue)
   \o #(room/door :simple false)
   \O #(room/door :simple true)
   \b #(entity/serv-bot)
   \- #(label (room/door :blast false) :blast-door)
   \| #(label (room/door :blast false) :blast-door)})

(def rooms {"starting-lab" {:tiles (merge common-tiles
                                     {\@ #(entity/player "Malefeitor" 100)
                                      \l #(weapon/lead-pipe)
                                      \- #(label (room/door :blast false) :blast-door)
                                      \c #(label (container/crate) :crate)})
                            :post (fn [{:keys [blast-door crate]}]
                                    (concat
                                      (compound/group (room/blast-door false) blast-door)
                                      (let [loot (weapon/dart-gun)]
                                        [loot (update (first crate) [::c/inventory] conj (::c/id loot))])))
                            :music :medical}})

(defn extract-room [s tiles [x0 y0] floor]
  (for [[y row] (util/enumerate (str/split-lines s))
        [x c] (util/enumerate row)
        :let [f (tiles c)] :when f]
    (position (f) (util/matrix-add [x0 y0] [x y]) floor)))

(defn load-room-resource [name]
  (or (io/resource (str "rooms/" name))
      (throw (ex-info (str "No such room resource found") {:name name}))))

(defn dimensions [level]
  (let [ps (map ::c/position (entity/filter-capable [::c/position] level))]
    [(inc (apply max (map first ps)))
     (inc (apply max (map second ps)))]))

(defn load-room [name [x0 y0] floor]
  (let [{:keys [tiles post music]} (rooms name)
        room (extract-room (slurp (load-room-resource name)) tiles [x0 y0] floor)
        [x1 y1] (util/matrix-add [x0 y0] (dimensions room))
        labeled (group-by ::c/label room)]
    (concat (if post
              (concat (get labeled nil) (post (dissoc labeled nil)))
              room)
      (for [x (range x0 x1) y (range y0 y1)]
        (position (entity/music music) [x y] floor)))))

;;; TODO index
(defn floor [game n]
  (let [es (world/capable-entities game ::c/foundation)]
    (or (first (filter #(= (::c/floor %) n) es))
        (throw (ex-info (format "No floor for level %d found" n) {:level n})))))

(defn entity-floor [game e]
  (floor game (::c/floor e)))

(defn in-bounds?
  "Is `[x y]` within the bounds of `foundation`?"
  [foundation [x y]]
  (let [[w h] foundation]
    (and (>= x 0) (>= y 0)
         (< x w) (< y h))))

(defn on-floor [floor es]
  (filter #(= (::c/floor %) floor) es))

(defn floor-entities [game floor]
  (on-floor floor (vals (world/entities game))))
