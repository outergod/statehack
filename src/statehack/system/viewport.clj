(ns statehack.system.viewport
  (:require [statehack.system.render :as render]
            [statehack.system.world :as world]
            [statehack.system.levels :as levels]
            [statehack.util :as util]))

(defn update-viewport [game e f]
  (let [{:keys [foundation]} (levels/entity-floor game e)
        {:keys [graphics]} game]
    (update-in game [:viewport] #(render/into-bounds graphics :world foundation (f %)))))

(defn center-viewport [game e]
  (update-viewport game e (constantly (render/center-on (:graphics game) (e :position)))))
