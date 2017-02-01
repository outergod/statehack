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

(ns statehack.system.world
  "World manipulation"
  (:refer-clojure :exclude [update])
  (:require [statehack.util :as util]
            [statehack.entity :as entity]
            [statehack.algebra :as algebra]))

;;; In order to implement the idea of a purely functional ECS, the current state
;;; of the world is always represented as a head of a sequence. Whenever a
;;; player action causes a new round in the game to start, the current world
;;; state is duplicated and this duplicate manipulated subsequently.

(def store
  "Storage for world state"
  (atom {}))

(defn save
  "Store the current world state into `store`"
  [game]
  (swap! store (constantly (:world game)))
  game)

(defn state
  "Current world state"
  [game]
  (-> game :world first))

(defn entities
  "All entities of current world state"
  [game]
  (:entities (state game)))

(defn entity
  "Lookup entity identified by `id` in current world state"
  [game id]
  ((entities game) id))

(defmacro update
  "Convenience macro to manipulate entities"
  {:arglists '([game [& ids] [& bindings] & updates] [game & updates])}
  [game & args]
  (let [[ids bindings & updates] (if (vector? (first args)) args (concat [nil nil] args))]
    `(reduce (fn [~game f#] (or (apply f# ~game (map (partial entity ~game) [~@ids])) ~game))
       ~game
       [~@(map (fn [body] `(fn [~game ~@bindings] ~body)) updates)])))

(defn dup-world-state
  "Duplicate and cons the current world state onto the state list"
  [game]
  (let [world (:world game)
        state (first world)]
    (assoc game :world (cons state world))))

(defn update-world-state
  "Update the current world state by running `apply f state args` against it"
  [game f & args]
  (update-in game [:world] (fn [[x & xs]] (cons (apply f x args) xs))))

(defn update-in-world-state
  "Run `update-in` with key sequence `ks` against the current world state"
  [game [& ks] f & args]
  (update-world-state game #(apply update-in % ks f args)))

(defn update-entity
  "Update entity identified by `id` in current state with `apply f entity args`"
  [game id f & args]
  (apply update-in-world-state game [:entities id] f args))

(defn remove-entity-component
  "Remove entity components from entity"
  [game id c & cs]
  (update-in-world-state game [:entities id] #(apply dissoc % c cs)))

(defn update-entity-component
  "Update entity components in entity

  `c` may be a sequence with additional keys, in which case the component itself
  is travesed and updated."
  [game id c f & args]
  (let [c (if (sequential? c) c [c])]
    (apply update-in-world-state game (concat [:entities id] c) f args)))

(defn add-entity-component
  "Add entity components"
  [game id c & cs]
  (update-in-world-state game [:entities id] #(apply merge % c cs)))

(defn update-entities
  "Update game entities with `apply f entities args`"
  [game f & args]
  (apply update-in-world-state game [:entities] f args))

(defn update-entities-component
  "Update component in all entities"
  [game c f args]
  (update-entities game #(into {} (map (fn [[k v]] [k (apply update-in c f args)]) %))))

(defn add-entity
  "Add entity to game"
  [game e]
  (update-entities game #(assoc % (:id e) e)))

(defn remove-entity
  "Remove identity from game"
  [game id]
  (update-entities game #(dissoc % id)))

(defn pop-world-state
  "Pop the current state of the world, if more than one exists"
  [game]
  (update-in game [:world] #(if (> (count %) 1) (next %) %)))

(defn entities-at
  "Matching entities at given `floor` and `coords`"
  ([game floor coords]
   (let [entities (entities game)
         coords (set coords)]
     (filter #(and (= (:floor %) floor) (coords (:position %)))
       (vals entities))))
  ([game e]
   (entities-at game (:floor e) [(:position e)])))

(defn direct-neighbors
  "Neighbor entities at given `floor` around `coords`"
  [game coords floor]
  (entities-at game floor (algebra/neighbors coords)))

(defn entity-neighbors
  "Neighbor entities of `e`"
  [game e]
  (direct-neighbors game (:position e) (:floor e)))

(defn capable-entities
  "Find entities owning components"
  [game & cs]
  (entity/filter-capable cs (vals (entities game))))

(defn entity-delta
  "Calculate delta between `e1` and `e2`"
  [e1 e2]
  (util/matrix-subtract (:position e1) (:position e2)))
