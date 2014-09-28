(ns statehack.system.input.receivers
  (:require [statehack.game.world :as world]))

(defn push-control [game e]
  (world/update-in-world-state game [:receivers] #(cons (:id e) %)))

(defn pop-control [game]
  (world/update-in-world-state game [:receivers] next))
