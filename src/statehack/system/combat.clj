(ns statehack.system.combat
  (:require [clojure.pprint :refer [cl-format]]
            [statehack.entity :as entity]
            [statehack.system.name :as name]
            [statehack.system.world :as world]
            [statehack.system.transition :as transition]
            [statehack.system.messages :as messages]
            [statehack.system.skills :as skills]))

(defn target-hp [e]
  (get-in e [:hp :current]))

(defn die [game e]
  (-> game
      (world/remove-entity-component e :mobile :obstacle :ai)
      (world/update-entity-component e :alive (constantly false))
      (world/update-entity-component e :renderable (constantly :corpse))
      (transition/transition #(transition/die))))

(defn dead? [e]
  (not (:alive e)))

(defn damage [game attacker amount target]
  (let [hp (- (target-hp target) amount)
        game (messages/log game (cl-format nil "~a attacks ~a, causing ~d damage" (name/name attacker) (name/name target) amount))]
    (if (pos? hp)
      (world/update-entity-component game target [:hp :current] - amount)
      (-> game
          (messages/log (cl-format nil "~a dies from ~d overdamage" (name/name target) (Math/abs hp)))
          (die target)))))

(defn- attackable? [e]
  (and (entity/capable? e :hp)
       (:alive e)))

(def melee-hierarchy (make-hierarchy))

(defn melee-dispatch [game attacker skill target] skill)

(defmulti melee #'melee-dispatch :hierarchy #'melee-hierarchy)

(defmethod melee :appendages [game attacker skill target]
  (let [game (transition/transition game (transition/sound :appendage-attack))
        strength (:strength (skills/entity-skill attacker skill))]
    (damage game attacker strength target)))

(defn available-melee [game e]
  (if-let [s (skills/any-type-skill e :melee)]
    (let [es (filter attackable? (world/entity-neighbors game e))]
      (into {} (map (fn [t] [(world/entity-delta t e) #(melee % e s t)]) es)))
    {}))
