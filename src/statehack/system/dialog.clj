(ns statehack.system.dialog
  (:require [statehack.entity.dialog :as dialog]
            [statehack.system.input.receivers :as receivers]
            [statehack.game.world :as world]))

(defn messages [game ms]
  {:pre [(coll? ms)]}
  (let [d (dialog/dialog ms)]
    (-> game (world/add-entity d) (receivers/push-control d))))

(defn message [game s]
  (messages game [s]))
