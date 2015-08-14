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
            [statehack.system.inventory :as inventory]
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

(defn act [game e f]
  (letfn [(center [game] (viewport/center-on game (world/entity game (:id e))))]
    (-> game world/dup-world-state f center time/pass-time)))

(defn action [game player dir]
  (let [[x y] (player-moves dir)
        moves (apply merge (map #(% game player)
                                (reverse [combat/available-melee door/available-open movement/available-moves])))
        non-move ((movement/unavailable-moves game player) [x y])]
    (if-let [m (moves [x y])]
      (act game player m)
      (non-move game))))

(defn viewport [game player dir]
  (viewport/update-viewport game player (partial util/matrix-add (player-moves dir))))

(defn pick-up [game player items]
  )

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
    \. (act game player identity)
    \, (pick-up game player (inventory/available-pickups game player))
    \C (door/close game player)
    :backspace (-> game world/pop-world-state (viewport/center-on player))
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
