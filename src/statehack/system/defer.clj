(ns statehack.system.defer
  (:require [statehack.entity.selector :as selector]
            [statehack.system.world :as world]
            [statehack.system.input.receivers :as receivers]))

(defn defer [game es action]
  (let [e (selector/selector (:position (first es)) action (map :id es))]
    (-> game (world/add-entity e) (receivers/push-control e))))

(defn fulfill [game e]
  (let [{:keys [deferred mobile]} e
        es (:entities (world/current-world-state game))
        t (es (first (:targets mobile)))]
    (-> game (world/remove-entity e) receivers/pop-control (deferred t))))

(defn abort [game e]
  (-> game (world/remove-entity e) receivers/pop-control))
