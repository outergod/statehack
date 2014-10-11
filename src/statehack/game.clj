(ns statehack.game
  (:require [statehack.system.world :as world]
            [statehack.system.input :as input]
            [statehack.system.render :as render]
            [statehack.util :as util]
            [statehack.entity.player :as player]
            [statehack.entity.status-bar :as status]
            [statehack.entity.bot :as bot]
            [statehack.entity.cursor :as cursor]
            [statehack.entity.room :as room]
            [statehack.entity.log :as log]
            [statehack.system.ai :as ai]
            [statehack.system.viewport :as viewport]
            [statehack.system.unique :as unique]
            [halo.screen :as screen]))

(def first-room
"XXXXXoXXXXX
X         X
X         X
X         X
X         X
X         X
XXXXXXXXXXX")

(defn new-game [screen]
  (let [{:keys [id] :as player} (player/player "Malefeitor" 40 18 10)]
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
                                          (room/extract-room first-room 35 13)
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

(defn run
  ([screen game]
     (doall (take-while identity (repeatedly #(screen/read-input screen))))
     (screen/in-screen screen
       (loop [input nil game (render/system game)]
         (screen/probe-resize screen)
         (let [[game {:keys [quit time]}] (-> game (input/player-turn input) render/system (util/separate :quit :time))]
           (when-not quit
             (let [game (if time (-> game ai/system render/system) game)]
               (recur (screen/read-input-blocking screen) game)))))))
  ([screen]
     (run screen (new-game screen))))
