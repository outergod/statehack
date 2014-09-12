(ns statehack.input
  (:require [statehack.entity :as entity]
            [statehack.game.world :as world]))

(defn- receive-dispatch [e game input]
  (:type e))

(defmulti receive #'receive-dispatch :hierarchy #'entity/entity-hierarchy)

(defn receiver [e]
  (assoc e :receiver true))

#_(defn push-receiver [game id]
  (let [state (world/current-world-state game)]
    (update-in game [:viewport] #(ui/into-bounds state scr (util/matrix-add % [x y]))))
  (world/update-world-state))

(defn- input-system-dispatch [game input]
  (:mode (world/current-world-state game)))

(defmulti input-system #'input-system-dispatch)

(defmethod input-system :world [game input]
  (let [{:keys [entities]} (world/current-world-state game)
        entities (filter :receiver (vals entities))]
    (reduce #(receive %2 %1 input) game entities)))
