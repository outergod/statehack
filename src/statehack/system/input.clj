(ns statehack.system.input
  (:require [statehack.system.input.receivers :as receivers]
            [statehack.system.world :as world]
            [statehack.system.viewport :as viewport]
            [statehack.system.movement :as movement]
            [statehack.system.door :as door]
            [statehack.system.combat :as combat]
            [statehack.system.defer :as defer]
            [statehack.system.time :as time]))

(def receive-hierarchy (make-hierarchy))

(defn derive-receive [tag parent]
  (alter-var-root #'receive-hierarchy derive tag parent))

(defn- receive-dispatch [game e input]
  (:input e))

(defmulti receive #'receive-dispatch :hierarchy #'receive-hierarchy)

(defn receiver [state]
  (let [{:keys [receivers entities]} state]
    (entities (first receivers))))

(defn player-turn [game input]
  (let [e (receiver (world/current-world-state game))]
    (-> (receive game e input) movement/update-cursor)))

(def player-moves
  {:up [0 -1]
   :down [0 1]
   :left [-1 0]
   :right [1 0]
   :up-left [-1 -1]
   :up-right [1 -1]
   :down-left [-1 1]
   :down-right [1 1]})

(defn action [game player dir]
  (let [[x y] (player-moves dir)
        moves (apply merge (map #(% game player)
                                (reverse [combat/available-melee door/available-open movement/available-moves])))
        non-move ((movement/unavailable-moves game player) [x y])]
    (if-let [m (moves [x y])]
      (-> game world/dup-world-state m (viewport/center-viewport player) time/pass-time)
      (non-move game))))

(defn viewport [game dir]
  (viewport/update-viewport game (player-moves dir)))

(defmethod receive :player [game player input]
  (case (:key input)
    :arrow-up (viewport game :up)
    :arrow-down (viewport game :down)
    :arrow-left (viewport game :left)
    :arrow-right (viewport game :right)
    \w (action game player :up)
    \s (action game player :down)
    \a (action game player :left)
    \d (action game player :right)
    \q (action game player :up-left)
    \e (action game player :up-right)
    \z (action game player :down-left)
    \c (action game player :down-right)
    \C (door/close game player)
    :backspace (-> game world/pop-world-state (viewport/center-viewport player))
    :enter (world/save game)
    :escape (assoc game :quit true)
    game))

(defmethod receive :selector [game selector input]
  (case (:key input)
    :arrow-up (viewport game :up)
    :arrow-down (viewport game :down)
    :arrow-left (viewport game :left)
    :arrow-right (viewport game :right)
    :tab (movement/move-next game selector)
    (:enter \ ) (defer/fulfill game selector)
    :escape (defer/abort game selector)
    game))

(defmethod receive :dialog [game dialog input]
  (case (:key input)
    (:enter \ ) (if (> (count (:messages dialog)) 1)
                  (world/update-entity-component game dialog :messages next)
                  (-> game receivers/pop-control (world/remove-entity dialog)))
    game))
