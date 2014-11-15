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
  (let [player-id (:id (unique/unique-entity game :player))
        visible-es (util/index-by :id (sight/visible-entities game e))
        memory-es (:entities (memory/entity-floor-memory e))]
    [(visible-es player-id) (memory-es player-id)]))

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
                            (map #(algebra/a* (:position e) % foundation os)
                                 (algebra/neighbors (:position target)))))]
    (if-let [path (first paths)]
      (movement/relocate game e (fnext path))
      game)))

(defmethod act :serv-bot [game e]
  (let [[type player] (player-known? game e)
        melee (skills/any-type-skill e :melee)]
    (world/>> game [(:id e)]
              #(when (and (= type :sight) (not (:player-spotted (memory/entity-memory %2))))
                 (-> %1
                     (transition/transition (transition/sound :serv-bot-spot))
                     (memory/update-memory %2 assoc :player-spotted true)))
              #(when (= (:position %2) (:position player))
                 (-> %1
                     (memory/update-memory-floor %2 (fn [mem] (update-in mem [:entities] dissoc (:id player))))
                     (memory/update-memory %2 dissoc :player-spotted)))
              #(cond (some-> %1 (player-nearby? %2) combat/attackable?) (combat/melee %1 %2 melee player)
                     (= type :sight) (move-melee-range %1 %2 player)
                     (= type :memory) (move-towards %1 %2 player)
                     :default (move-random %1 %2)))))

(defn system [game]
  (let [es (world/capable-entities game :ai)]
    (reduce act game es)))
