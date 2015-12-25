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

(ns statehack.system.ai
  (:require [statehack.system.world :as world]
            [statehack.system.movement :as movement]
            [statehack.system.door :as door]
            [statehack.system.combat :as combat]
            [statehack.system.memory :as memory]
            [statehack.system.sight :as sight]
            [statehack.system.unique :as unique]
            [statehack.system.transition :as transition]
            [statehack.system.skills :as skills]
            [statehack.system.levels :as levels]
            [statehack.algebra :as algebra]
            [statehack.util :as util]))

(def act-hierarchy (make-hierarchy))

(defn act-dispatch [game e]
  (-> e :ai :type))

(defmulti act #'act-dispatch :hierarchy #'act-hierarchy)

(defn known-player-location [game e]
  (let [visible-es (util/index-by :id (sight/visible-entities game e))]
    [(visible-es (:id (unique/unique-entity game :player)))
     (:player (memory/entity-memory e))]))

(defn first-player-spot? [game e]
  (let [[sight memory] (known-player-location game e)]
    (and sight (not memory))))

(defn player-known? [game e]
  (let [[sight memory] (known-player-location game e)]
    (cond sight [:sight sight]
          memory [:memory memory])))

(defn player-nearby? [game e]
  (let [player-id (:id (unique/unique-entity game :player))
        es (util/index-by :id (world/entity-neighbors game e))]
    (es player-id)))

(defn move-random [game e]
  (let [moves (vals (movement/available-moves game e))]
    (if-let [move (rand-nth moves)]
      (move game)
      game)))

(defn path-to [game e target]
  (let [{:keys [foundation]} (levels/entity-floor game e)
        es (vals (dissoc (:entities (memory/entity-floor-memory e)) (:id target)))
        os (set (map :position es))]
    (next (algebra/a* (:position e) (:position target) foundation os))))

(defn move-towards [game e target]
  (let [{:keys [foundation]} (levels/entity-floor game e)
        es (vals (dissoc (:entities (memory/entity-floor-memory e)) (:id target)))
        os (set (map :position es))
        path (algebra/a* (:position e) (:position target) foundation os)]
    (if (> (count path) 1)
      (movement/relocate game e (fnext path))
      game)))

(defn move-melee-range [game e target]
  (let [{:keys [foundation]} (levels/entity-floor game e)
        es (vals (:entities (memory/entity-floor-memory e)))
        os (set (map :position es))
        paths (sort algebra/PathComparator
                    (remove nil?
                            (pmap #(algebra/a* (:position e) % foundation os)
                                  (algebra/neighbors (:position target)))))]
    (if-let [path (first paths)]
      (movement/relocate game e (fnext path))
      game)))

(defn forget-player [game e player]
  (memory/update-memory game e dissoc :player :player-spotted?))

(defmethod act :serv-bot [game e]
  (let [[type player] (player-known? game e)
        melee (skills/any-type-skill e :melee)]
    (world/>> game [(:id e)]
              #(when (and (= type :sight) (not (:player-spotted? (memory/entity-memory %2))))
                 (-> %1
                     (transition/transition (transition/sound :serv-bot-spot))
                     (memory/update-memory %2 assoc :player-spotted? true)))
              #(when (= (:position %2) (:position player))
                 (forget-player %1 %2 player))
              #(cond (some-> %1 (player-nearby? %2) combat/attackable?) (combat/melee %1 %2 melee player)
                     (= type :sight) (-> %1
                                         (move-melee-range %2 player)
                                         (memory/update-memory %2 assoc :player player))
                     (= type :memory) (if-let [path (path-to %1 %2 player)]
                                        (movement/relocate %1 %2 (first path))
                                        (forget-player %1 %2 player))
                     :default (move-random %1 %2)))))

(defn system [game]
  (let [es (world/capable-entities game :ai)]
    (reduce act game es)))
