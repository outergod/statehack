(ns statehack.system.status
  (:require [clojure.pprint :refer [cl-format]]
            [statehack.entity :as entity]
            [statehack.system.name :as name]
            [statehack.system.world :as world]
            [statehack.system.unique :as unique]
            [statehack.system.messages :as messages]))

(defn player-status [e]
  (let [{:keys [adaptive hp]} e
        {:keys [current max]} hp
        hp-order (inc (int (Math/log10 max)))]
    (cl-format nil (str "~a | HP: ~" hp-order "d/~d | XP: ~d | Level: ~d") (name/name e) current max (:xp adaptive) (:level adaptive))))

(defn text [game e]
  (let [p (unique/unique-entity game :player)]
    (if (entity/capable? e :messages)
      (messages/current e)
      (player-status p))))
