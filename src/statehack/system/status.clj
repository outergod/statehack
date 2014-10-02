(ns statehack.system.status
  (:require [clojure.pprint :refer [cl-format]]
            [statehack.entity :as entity]
            [statehack.system.world :as world]
            [statehack.system.dialog :as dialog]))

(defn player-status [e]
  (let [{:keys [name adaptive hp]} e
        {:keys [current max]} hp
        hp-order (inc (int (Math/log10 max)))]
    (cl-format nil (str "~a | HP: ~" hp-order "d/~d | XP: ~d | Level: ~d") name current max (:xp adaptive) (:level adaptive))))

(defn text [game e]
  (let [p (world/singular-entity game :player)]
    (if (entity/capable? e :messages)
      (dialog/current e)
      (player-status p))))
