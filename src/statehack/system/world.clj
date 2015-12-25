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

(ns statehack.system.world
  (:require [statehack.util :as util]
            [statehack.entity :as entity]
            [statehack.algebra :as algebra]))

(def store (atom {}))

(defn save [game]
  (swap! store (constantly (:world game)))
  game)

(defn state [game]
  (-> game :world first))

(defn entities [game]
  (:entities (state game)))

(defn entity [game id]
  ((entities game) id))

(defmacro >> [game [& ids] [& bindings] & updates]
  {:pre [(= (count bindings) (count ids))]}
  `(reduce (fn [~game f#] (or (apply f# ~game (map (partial entity ~game) [~@ids])) ~game))
           ~game
           [~@(map (fn [body] `(fn [~game ~@bindings] ~body)) updates)]))

(defn dup-world-state [game]
  (let [world (:world game)
        state (first world)]
    (assoc game :world (cons state world))))

(defn update-world-state [game f & args]
  (update-in game [:world] (fn [[x & xs]] (cons (apply f x args) xs))))

(defn update-in-world-state [game [& ks] f & args]
  (update-world-state game #(apply update-in % ks f args)))

(defn update-entity [game e f & args]
  (apply update-in-world-state game [:entities (:id e)] f args))

(defn remove-entity-component [game e c & cs]
  (update-in-world-state game [:entities (:id e)] #(apply dissoc % c cs)))

(defn update-entity-component [game e c f & args]
  (let [c (if (sequential? c) c [c])]
    (apply update-in-world-state game (concat [:entities (:id e)] c) f args)))

(defn add-entity-component [game e c & cs]
  (update-in-world-state game [:entities (:id e)] #(apply merge % c cs)))

(defn update-entities [game f & args]
  (apply update-in-world-state game [:entities] f args))

(defn update-entities-component [game c f args]
  (update-entities game #(into {} (map (fn [[k v]] [k (apply update-in c f args)]) %))))

(defn add-entity [game e]
  (update-entities game #(assoc % (:id e) e)))

(defn remove-entity [game e]
  (update-entities game #(dissoc % (:id e))))

(defn pop-world-state [game]
  (update-in game [:world] #(if (> (count %) 1) (next %) %)))

(defn entities-at
  ([game floor coords]
   (let [entities (entities game)
         coords (set coords)]
     (filter #(and (= (:floor %) floor) (coords (:position %)))
             (vals entities))))
  ([game e]
   (entities-at game (:floor e) [(:position e)])))

(defn direct-neighbors [game [x y] floor]
  (entities-at game floor (algebra/neighbors [x y])))

(defn entity-neighbors [game e]
  (direct-neighbors game (:position e) (:floor e)))

(defn capable-entities [game & cs]
  (entity/filter-capable cs (vals (entities game))))

(defn entity-delta [e1 e2]
  (util/matrix-subtract (:position e1) (:position e2)))
