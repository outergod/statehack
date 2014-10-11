(ns statehack.system.combat
  (:require [clojure.pprint :refer [cl-format]]
            [statehack.entity :as entity]
            [statehack.system.name :as name]
            [statehack.system.world :as world]
            [statehack.system.transition :as transition]
            [statehack.system.messages :as messages]))

(defn target-hp [e]
  (get-in e [:hp :current]))

(defn die [game e]
  (-> game
      (world/remove-entity-component e :mobile :obstacle :ai)
      (world/update-entity-component e :alive (constantly false))
      (world/update-entity-component e :renderable (constantly :corpse))))

(defn dead? [e]
  (not (:alive e)))

(defn damage [game attacker skill target]
  (let [hp (- (target-hp target) skill)
        game (-> game
                 (transition/transition #(transition/punch))
                 (messages/log (cl-format nil "~a attacks ~a, causing ~d damage" (name/name attacker) (name/name target) skill)))]
    (if (pos? hp)
      (world/update-entity-component game target [:hp :current] - skill)
      (-> game
          (messages/log (cl-format nil "~a dies from ~d overdamage" (name/name target) (Math/abs hp)))
          (die target)))))

(defn- attackable? [e]
  (and (entity/capable? e :hp)
       (:alive e)))

(defn available-melee [game e]
  (let [es (filter attackable? (world/entity-neighbors game e))]
    (into {} (map (fn [t] [(world/entity-delta t e) #(damage % e 1 t)]) es))))
