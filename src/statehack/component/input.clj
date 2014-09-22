(ns statehack.component.input
  (:require [statehack.game.world :as world]))

(def receive-hierarchy (make-hierarchy))

(defn derive-receive [tag parent]
  (alter-var-root #'receive-hierarchy derive tag parent))

(defn receiver [e type]
  (assoc e :receiver type))

(defn- receive-dispatch [e game input]
  (:receiver e))

(defmulti receive #'receive-dispatch :hierarchy #'receive-hierarchy)

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
