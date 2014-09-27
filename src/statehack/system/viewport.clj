(ns statehack.system.viewport
  (:require [statehack.system.render :as render]
            [statehack.game.world :as world]
            [statehack.util :as util]))

(defn update-viewport [game [x y]]
  (let [state (world/current-world-state game)
        screen (:screen game)]
    (update-in game [:viewport] #(render/into-bounds state screen (util/matrix-add % [x y])))))

(defn set-viewport [game [x y]]
  (let [state (world/current-world-state game)
        foundation (:foundation state)
        screen (:screen game)]
    (update-in game [:viewport] (constantly (render/into-bounds foundation screen [x y])))))

(defn center-viewport [game e]
  (let [screen (:screen game)]
    (set-viewport game (render/center screen (e :position)))))
