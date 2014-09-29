(ns statehack.game
  (:require [statehack.system.input :as input]
            [statehack.system.render :as render]
            [statehack.util :as util]
            [statehack.entity.player :as player]
            [statehack.entity.cursor :as cursor]
            [statehack.entity.room :as room]
            [lanterna.screen :as screen]))

(def first-room
"XoXXXoXXXXX
o         X
X         X
X         X
X         X
X         X
XXXXXXXXXXX")

(defn new-game [scr]
  (let [{:keys [id] :as player} (player/player 40 18)]
    {:screen scr
     :viewport [0 0]
     :world [{:foundation (render/space 80 24)
              :player id
              :receivers [id]
              :entities (util/index-by :id
                                       (flatten
                                        [player
                                         (cursor/cursor 0 0 {:follow id})
                                         (room/extract-room first-room 35 13)]))}]}))

(defn load-game [scr world]
  {:screen scr
   :viewport [0 0]
   :world world})

(comment
  (game/run scr (game/load-game scr @statehack.game.world/state)))

(defn run
  ([screen game]
     (doall (take-while identity (repeatedly #(screen/get-key screen))))
     (screen/in-screen screen (input/system game)))
  ([screen]
     (run screen (new-game screen))))
