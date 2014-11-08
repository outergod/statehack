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
            [statehack.util :as util]))

(def act-hierarchy (make-hierarchy))

(defn act-dispatch [game e]
  (-> e :ai :type))

(defmulti act #'act-dispatch :hierarchy #'act-hierarchy)

(defn known-player-location [game e]
  (let [player-id (:id (unique/unique-entity game :player))
        visible-es (util/index-by :id (sight/visible-entities game e))
        memory-es (:entities (memory/entity-memory e))]
    [(visible-es player-id) (memory-es player-id)]))

(defn first-player-spot? [game e]
  (let [[sight memory] (known-player-location game e)]
    (and sight (not memory))))

(defn player-known? [game e]
  (let [[sight memory] (known-player-location game e)]
    (or sight memory)))

(defn player-nearby? [game e]
  (let [player-id (:id (unique/unique-entity game :player))
        es (util/index-by :id (world/entity-neighbors game e))]
    (es player-id)))

(defn move-random [game e]
  (let [moves (vals (movement/available-moves game e))]
    (if-let [move (rand-nth moves)]
      (move game)
      game)))

(defmethod act :serv-bot [game e]
  (let [player (unique/unique-entity game :player)
        melee (skills/any-type-skill e :melee)]
    (world/>> game
              #(when (first-player-spot? % e)
                 (transition/transition % (transition/sound :serv-bot-spot)))
              #(if (some-> % (player-nearby? e) combat/attackable?)
                 (combat/melee % e melee player)
                 (move-random % e)))))

(defn system [game]
  (let [es (world/capable-entities game :ai)]
    (reduce act game es)))
