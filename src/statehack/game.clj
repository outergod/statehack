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

(ns statehack.game
  "Main game loop"
  (:require [statehack.system.world :as world]
            [statehack.system.player :as player]
            [statehack.system.layout :as layout]
            [statehack.system.graphics :as graphics]
            [statehack.entity :as entity]
            [statehack.system.ai :as ai]
            [statehack.system.viewport :as viewport]
            [statehack.system.unique :as unique]
            [statehack.system.messages :as messages]
            [statehack.system.combat :as combat]
            [statehack.system.transition :as transition]
            [statehack.system.levels :as levels]
            [statehack.system.memory :as memory]
            [statehack.system.sound :as sound]
            [statehack.system.input.receivers :as receivers]
            [statehack.system.unique :as unique]
            [statehack.util :as util]
            [halo.screen :as screen]))

(defn new-game
  "Create new game from scratch"
  [screen]
  (let [;level (levels/load "level-0" 1)
        lab (levels/load-room "starting-lab" [0 0] 1)
        [w h] (levels/dimensions lab)
        game {:screen screen
              :graphics (screen/text-graphics screen)
              :world [{:entities (util/index-by :id
                                                (concat
                                                  [(entity/floor 1 [w h])
                                                   (entity/status-bar)
                                                   (entity/cursor)
                                                   (entity/log)]
                                                  lab))}]}]
    (receivers/push-control game (unique/unique-entity game :player))))

(defn load-game
  "Load the game from given `world`"
  [screen world]
  {:screen screen
   :graphics (screen/text-graphics screen)
   :world world})

(defn game-over
  "Transition the game to game over state"
  [game]
  (let [{:keys [screen]} game]
    (-> game (messages/log "Game Over. Whatever that means..") graphics/system)
    (screen/read-input-blocking screen)))

(defn turn
  "Run all relevant systems after a player or AI action has been processed"
  [game]
  (-> game transition/system memory/system layout/system graphics/system))

(defn run
  "Main game loop

  Purge the current input queue, init sound and start the actual game."
  [{:keys [screen] :as game}]
  (doall (take-while identity (repeatedly #(screen/read-input screen))))
  (sound/init)
  (try (screen/in-screen screen
         (loop [input nil game (-> game memory/system layout/system sound/music-system (viewport/center-on (unique/unique-entity game :player)) graphics/system)]
           (screen/probe-resize screen)
           (let [[game {:keys [quit time]}] (-> game (player/system input) turn (util/separate :quit :time))]
             (when-not quit
               (let [game (sound/music-system (if time (-> game ai/system turn) game))
                     player (unique/unique-entity game :player)]
                 (if (combat/dead? player)
                   (game-over game)
                   (recur (screen/read-input-blocking screen) game)))))))
       (finally (sound/cleanup))))
