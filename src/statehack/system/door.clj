(ns statehack.system.door
  (:require [statehack.entity :as entity]
            [statehack.entity.selector :as selector]
            [statehack.system.messages :as messages]
            [statehack.system.defer :as defer]
            [statehack.system.input.receivers :as receivers]
            [statehack.system.world :as world]
            [statehack.system.time :as time]
            [statehack.system.transition :as transition]
            [clojure.set :as set]))

(defn open-door [game e]
  (-> game
      (world/update-entity-component e :open (constantly true))
      (transition/transition transition/door)))

(defn available-open [game e]
  (let [es (filter #(and (entity/capable? % :open)
                         (not (:open %)))
                   (world/entity-neighbors game e))]
    (into {} (map (fn [door] [(world/entity-delta door e) #(open-door % door)]) es))))

(defn close-door [game e]
  (-> game (world/update-entity-component e :open (constantly false)) (transition/transition transition/door) time/pass-time))

(defn available-close [game e]
  (let [es (filter #(and (entity/capable? % :open)
                         (:open %))
                   (world/entity-neighbors game e))]
    (into {} (map (fn [door] [(world/entity-delta door e) #(close-door % door)]) es))))

(defn close [game e]
  (let [es (filter #(and (entity/capable? % :open) (:open %))
                   (world/entity-neighbors game e))]
    (case (count es)
      0 (messages/log game "No open door nearby.")
      1 (close-door game (first es))
      (defer/defer game es close-door))))
