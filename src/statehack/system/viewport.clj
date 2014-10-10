(ns statehack.system.viewport
  (:require [statehack.system.render :as render]
            [statehack.system.world :as world]
            [statehack.util :as util]))

(defn update-viewport [game f]
  (let [state (world/state game)
        foundation (:foundation state)
        {:keys [graphics]} game]
    (update-in game [:viewport] #(render/into-bounds graphics :world foundation (f %)))))

(defn center-viewport [game e]
  (update-viewport game (constantly (render/center-on (:graphics game) (e :position)))))
