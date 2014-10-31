(ns statehack.system.ai
  (:require [statehack.system.world :as world]
            [statehack.system.movement :as movement]
            [statehack.system.door :as door]
            [statehack.system.combat :as combat]))

(def act-hierarchy (make-hierarchy))

(defn act-dispatch [game e]
  (-> e :ai :type))

(defmulti act #'act-dispatch :hierarchy #'act-hierarchy)

(defmethod act :serv-bot [game e]
  (let [ms (combat/available-melee game e)]
    (if-let [[_ m] (first ms)]
      (m game)
      game)))

(defn system [game]
  (let [es (world/capable-entities game :ai)]
    (reduce act game es)))
