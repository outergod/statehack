(ns statehack.system.input.receivers
  (:require [statehack.system.world :as world]))

(defn current [game]
  (let [{:keys [entities receivers]} (world/state game)]
    (entities (first receivers))))

(defn push-control [game e]
  (world/update-in-world-state game [:receivers] #(cons (:id e) %)))

(defn pop-control [game]
  (world/update-in-world-state game [:receivers] next))
