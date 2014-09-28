(ns statehack.component.door
  (:require [statehack.entity :as entity]
            [statehack.game.world :as world]))

(defn toggle-door-dispatch [game actor reactor open]
  [(:type actor) (:type reactor)])

(defmulti toggle-door #'toggle-door-dispatch :hierarchy #'entity/entity-hierarchy)
(defmethod toggle-door [:player :door] [game player door open?]
  [game player (assoc door :open open?)])

(defn close-candidates [game x y]
  (letfn [(isa-door? [e] (:open (keys e)))]
    (seq (filter (every-pred isa-door? :open)
                 (world/direct-neighbors (world/current-world-state game) x y)))))
