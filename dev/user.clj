(ns user
  (:require [lanterna.screen :as screen]
            [statehack.game :as game]
            [statehack.entity :as entity]
            [statehack.game.world :as world]
            [statehack.system.input :as input]
            [statehack.system.movement :as movement]
            [statehack.system.door :as door]))

(def scr (screen/get-screen :text))

(comment
  (game/run scr)

  (let [game (game/load-game scr @world/state)
        player (world/player-entity game)]
    (movement/available-moves game player)))
