(ns statehack.game
  (:require [statehack.system.world :as world]
            [statehack.system.input :as input]
            [statehack.system.render :as render]
            [statehack.util :as util]
            [statehack.entity.player :as player]
            [statehack.entity.status-bar :as status]
            [statehack.entity.bot :as bot]
            [statehack.entity.cursor :as cursor]
            [statehack.entity.log :as log]
            [statehack.system.ai :as ai]
            [statehack.system.viewport :as viewport]
            [statehack.system.unique :as unique]
            [statehack.system.messages :as messages]
            [statehack.system.combat :as combat]
            [statehack.system.transition :as transition]
            [statehack.system.levels :as levels]
            [halo.screen :as screen]))

(defn new-game [screen]
  (let [{:keys [id] :as player} (player/player "Malefeitor" [12 7] 10)]
    (viewport/center-viewport
     {:screen screen
      :graphics (screen/text-graphics screen)
      :world [{:foundation #_(render/space 7 [80 24]) (render/space 7 [500 500])
               :receivers [id]
               :entities (util/index-by :id
                                        (flatten
                                         [player
                                          (status/status-bar)
                                          (cursor/cursor)
                                          (log/log)
                                          (levels/load "level-0" [0 0])
                                          (bot/bot 40 10 5)]))}]}
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
    (-> game (messages/log "Game Over. Whatever that means..") render/system)
    (screen/read-input-blocking screen)))

(defn run
  ([screen game]
     (doall (take-while identity (repeatedly #(screen/read-input screen))))
     (screen/in-screen screen
       (loop [input nil game (render/system game)]
         (screen/probe-resize screen)
         (let [[game {:keys [quit time]}] (-> game (input/player-turn input) transition/system render/system (util/separate :quit :time))]
           (when-not quit
             (let [game (if time (-> game ai/system transition/system render/system) game)
                   player (unique/unique-entity game :player)]
               (if (combat/dead? player)
                 (game-over game)
                 (recur (screen/read-input-blocking screen) game))))))))
  ([screen]
     (run screen (new-game screen))))
