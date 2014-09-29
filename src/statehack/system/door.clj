(ns statehack.system.door
  (:require [statehack.entity :as entity]
            [statehack.game.world :as world]
            [clojure.set :as set]))

(defn open-door [game e]
  (world/update-entity-component game e :open (constantly true)))

(defn available-open [game e]
  (let [es (filter #(and (entity/capable? % :open)
                         (not (:open %)))
                   (world/entity-neighbors game e))]
    (into {} (map (fn [door] [(world/entity-delta door e) #(open-door % door)]) es))))
