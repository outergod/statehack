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

(ns statehack.system.levels
  (:refer-clojure :exclude [load])
  (:require [statehack.entity :as entity]
            [statehack.entity.room :as room]
            [statehack.system.world :as world]
            [statehack.util :as util]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn load-resource [name]
  (or (io/resource (str "levels/" name))
      (throw (ex-info (str "No such level resource found") {:name name}))))

(defn extract [s [x0 y0] floor]
   (let [lines (str/split-lines s)
         find-wall (memoize (fn [[x y]] (#{\X \O \o \e} (get-in lines [y x]))))]
     (filter identity
             (flatten
              (for [[row y] (partition 2 (interleave lines (range (count s))))
                    [token x] (partition 2 (interleave row (range (count row))))]
                (when-let [w (find-wall [x y])]
                  (let [[x1 y1] (util/matrix-add [x0 y0] [x y])]
                    (case w
                      \X (room/wall [x1 y1 floor])
                      \O (room/door [x1 y1 floor] true)
                      \o (room/door [x1 y1 floor] false)
                      \e (room/solid [x1 y1 floor])))))))))

(defn load [name floor]
  (extract (slurp (load-resource name)) [0 0] floor))

(defn dimensions [level]
  (let [ps (map :position (entity/filter-capable [:position] level))]
    [(inc (apply max (map first ps)))
     (inc (apply max (map second ps)))]))

(defn floor [game n]
  (let [es (world/capable-entities game :foundation)]
    (or (first (filter #(= (:floor %) n) es))
        (throw (ex-info (format "No floor for level %d found" n) {:level n})))))

(defn entity-floor [game e]
  {:pre [(:floor e)]}
  (floor game (:floor e)))

(defn in-bounds?
  "Is `[x y]` within the bounds of `foundation`?"
  [foundation [x y]]
  (let [[w h] foundation]
    (and (>= x 0) (>= y 0)
         (< x w) (< y h))))

(defn on-floor [floor es]
  (filter #(= (:floor %) floor) es))

(defn floor-entities [game floor]
  (on-floor floor (vals (world/entities game))))
