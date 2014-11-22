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

(ns statehack.system.memory
  "Memory system"
  (:require [statehack.system.levels :as levels]
            [statehack.system.sight :as sight]
            [statehack.system.world :as world]
            [statehack.util :as util]
            [clojure.set :as set]))

(defn entity-memory
  [e] (:memory e))

(defn update-memory [game e f & args]
  (apply world/update-entity-component game e :memory f args))

(defn entity-floor-memory
  ([e n] (get-in e [:memory :floors n] {:entities {} :coordinates []}))
  ([e] (entity-floor-memory e (:floor e))))

(defn update-memory-floor
  ([game e n f]
     (update-memory game e #(update-in % [:floors n] f)))
  ([game e f]
     (update-memory-floor game e (:floor (levels/entity-floor game e)) f)))

(defn remember-view [game e]
  {:pre [(:sight e) (:memory e) (:floor e)]}
  (let [{:keys [floor]} (levels/entity-floor game e)
        mask (sight/visible-mask game e)
        es (dissoc (util/index-by :id (sight/visible-entities game floor mask)) (e :id))]
    (update-memory-floor game e floor
                         #(-> %
                              (update-in [:entities] merge es)
                              (update-in [:coordinates] set/union mask)))))

(defn system [game]
  (let [es (world/capable-entities game :memory :sight :floor)]
    (reduce remember-view game es)))
