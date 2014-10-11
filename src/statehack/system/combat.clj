(ns statehack.system.combat
  (:require [clojure.pprint :refer [cl-format]]
            [statehack.entity :as entity]
            [statehack.system.name :as name]
            [statehack.system.world :as world]
            [statehack.system.sound :as sound]
            [statehack.system.messages :as messages]))

(defn target-hp [e]
  (get-in e [:hp :current]))

(defn damage [game attacker skill target]
  (let [hp (- (target-hp target) skill)
        game (messages/log game (cl-format nil "~a attacks ~a, causing ~d damage" (name/name attacker) (name/name target) skill))]
    (sound/play :punch-02)
    (if (pos? hp)
      (world/update-entity-component game target [:hp :current] - skill)
      (-> game
          (messages/log (cl-format nil "~a dies" (name/name target)))
          (world/remove-entity target)))))

(defn available-melee [game e]
  (let [es (entity/filter-capable [:hp] (world/entity-neighbors game e))]
    (into {} (map (fn [t] [(world/entity-delta t e) #(damage % e 1 t)]) es))))
