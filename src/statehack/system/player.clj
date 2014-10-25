(ns statehack.system.player
  (:require [statehack.system.input :as input]
            [statehack.system.input.receivers :as receivers]
            [statehack.system.world :as world]
            [statehack.system.viewport :as viewport]
            [statehack.system.movement :as movement]
            [statehack.system.door :as door]
            [statehack.system.combat :as combat]
            [statehack.system.defer :as defer]
            [statehack.system.time :as time]
            [statehack.system.messages :as messages]
            [statehack.util :as util]))

(def player-moves
  {:up [0 -1]
   :down [0 1]
   :left [-1 0]
   :right [1 0]
   :up-left [-1 -1]
   :up-right [1 -1]
   :down-left [-1 1]
   :down-right [1 1]})

(defn system [game input]
  (let [e (receivers/current game)]
    (-> (input/receive game e input) movement/update-cursor)))

(defn action [game player dir]
  (let [[x y] (player-moves dir)
        moves (apply merge (map #(% game player)
                                (reverse [combat/available-melee door/available-open movement/available-moves])))
        non-move ((movement/unavailable-moves game player) [x y])
        center #(viewport/center-viewport % (world/entity % (:id player)))]
    (if-let [m (moves [x y])]
      (-> game world/dup-world-state m center time/pass-time)
      (non-move game))))

(defn viewport [game player dir]
  (viewport/update-viewport game player (partial util/matrix-add (player-moves dir))))

(defmethod input/receive :player [game player input]
  (case (:key input)
    :arrow-up (viewport game player :up)
    :arrow-down (viewport game player :down)
    :arrow-left (viewport game player :left)
    :arrow-right (viewport game player :right)
    \w (action game player :up)
    \x (action game player :down)
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

(defmethod input/receive :selector [game selector input]
  (case (:key input)
    :arrow-up (viewport game :up)
    :arrow-down (viewport game :down)
    :arrow-left (viewport game :left)
    :arrow-right (viewport game :right)
    :tab (movement/move-next game selector)
    (:enter \ ) (defer/fulfill game selector)
    :escape (defer/abort game selector)
    game))

(defmethod input/receive :dialog [game dialog input]
  (case (:key input)
    (:enter \ ) (if (> (count (:messages dialog)) 1)
                  (messages/pop dialog)
                  (-> game receivers/pop-control (world/remove-entity dialog)))
    game))
