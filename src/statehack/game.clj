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

(ns statehack.game
  (:require [statehack.system.world :as world]
            [statehack.system.player :as player]
            [statehack.system.layout :as layout]
            #_[statehack.system.render :as render]
            [statehack.entity.floor :as floor]
            [statehack.entity.player :as player-entity]
            [statehack.entity.status-bar :as status]
            [statehack.entity.serv-bot :as serv-bot]
            [statehack.entity.cursor :as cursor]
            [statehack.entity.log :as log]
            [statehack.system.ai :as ai]
            [statehack.system.viewport :as viewport]
            [statehack.system.unique :as unique]
            [statehack.system.messages :as messages]
            [statehack.system.combat :as combat]
            [statehack.system.transition :as transition]
            [statehack.system.levels :as levels]
            [statehack.system.memory :as memory]
            [statehack.system.sound :as sound]
            [statehack.util :as util]
            [halo.screen :as screen]))

(defn new-game [screen]
  (let [;level (levels/load "level-0" 1)
        lab (levels/load-room "starting-lab" [0 0] 1)
        [w h] (levels/dimensions lab)
        {:keys [id] :as player} (player-entity/player "Malefeitor" [12 7 1] 100)]
    (viewport/center-viewport
     {:screen screen
      :graphics (screen/text-graphics screen)
      :world [{:receivers [id]
               :entities (util/index-by :id
                                        (concat
                                         [player
                                          (floor/floor 1 [w h])
                                          (status/status-bar)
                                          (cursor/cursor)
                                          (log/log)]
                                         lab))}]}
     player)))

(defn load-game [screen world]
  (let [game {:screen screen
              :graphics (screen/text-graphics screen)
              :world world}
        player (unique/unique-entity game :player)]
    (viewport/center-viewport game player)))

(comment
  (game/run scr (game/load-game scr @statehack.system.world/state)))

(defn game-over [game]
  (let [{:keys [screen]} game]
    (-> game (messages/log "Game Over. Whatever that means..") #_render/system)
    (screen/read-input-blocking screen)))

;; TODO render

(defn turn [game]
  (-> game transition/system memory/system layout/system #_render/system))

(defn run
  ([screen game]
     (doall (take-while identity (repeatedly #(screen/read-input screen))))
     (sound/init)
     (screen/in-screen screen
       (loop [input nil game (-> game memory/system layout/system #_render/system)]
         (screen/probe-resize screen)
         (let [[game {:keys [quit time]}] (-> game (player/system input) turn (util/separate :quit :time))]
           (when-not quit
             (let [game (if time (-> game ai/system turn) game)
                   player (unique/unique-entity game :player)]
               (if (combat/dead? player)
                 (game-over game)
                 (recur (screen/read-input-blocking screen) game))))))))
  ([screen]
     (run screen (new-game screen))))
