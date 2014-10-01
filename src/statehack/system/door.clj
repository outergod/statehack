(ns statehack.system.door
  (:require [statehack.entity :as entity]
            [statehack.entity.selector :as selector]
            [statehack.system.dialog :as dialog]
            [statehack.system.input.receivers :as receivers]
            [statehack.game.world :as world]
            [clojure.set :as set]))

(defn open-door [game e]
  (world/update-entity-component game e :open (constantly true)))

(defn available-open [game e]
  (let [es (filter #(and (entity/capable? % :open)
                         (not (:open %)))
                   (world/entity-neighbors game e))]
    (into {} (map (fn [door] [(world/entity-delta door e) #(open-door % door)]) es))))

(defn close-door [game e]
  (world/update-entity-component game e :open (constantly false)))

(defn available-close [game e]
  (let [es (filter #(and (entity/capable? % :open)
                         (:open %))
                   (world/entity-neighbors game e))]
    (into {} (map (fn [door] [(world/entity-delta door e) #(close-door % door)]) es))))

(defn close-selector [game e]
  (if-let [es (seq (filter #(and (entity/capable? % :open) (:open %))
                           (world/entity-neighbors game e)))]
    (let [e (selector/selector (map :id es))]
      (-> game (world/add-entity e) (receivers/push-control e)))
    (dialog/message game "No open door nearby.")))
