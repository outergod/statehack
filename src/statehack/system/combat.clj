(ns statehack.system.combat
  (:require [clojure.pprint :refer [cl-format]]
            [statehack.entity :as entity]
            [statehack.system.name :as name]
            [statehack.system.world :as world]
            [statehack.system.sound :as sound]))

(defn target-hp [e]
  (get-in e [:hp :current]))

(defn damage [game attacker skill target]
  (let [hp (- (target-hp target) skill)]
    (sound/play :punch-02)
    (if (pos? hp)
      (do
        (cl-format true "~a attacks ~a, damaging it by ~d~%" (name/name attacker) (name/name target) skill)
        (world/update-entity-component game target [:hp :current] - skill))
      (do
        (cl-format true "~a kills ~a, damaging it by ~d~%" (name/name attacker) (name/name target) skill)
        (world/remove-entity game target)))))

(defn available-melee [game e]
  (let [es (entity/filter-capable [:hp] (world/entity-neighbors game e))]
    (into {} (map (fn [t] [(world/entity-delta t e) #(damage % e 1 t)]) es))))
