(ns statehack.system.movement
  (:require [statehack.entity :as entity]
            [statehack.system.render :as render]
            [statehack.system.viewport :as viewport]
            [statehack.system.dialog :as dialog]
            [statehack.game.world :as world]
            [statehack.util :as util]))

(def move-hierarchy (make-hierarchy))

(defn- move-dispatch [e game x y]
  (:mobile e))

(defmulti move #'move-dispatch :hierarchy #'move-hierarchy)

(defn can-move? [es]
  (every? identity (map :open es)))

(defn move-into [game e1 e2]
  (println e1 e2 (entity/capable? e2 :open))
  (cond (entity/capable? e2 :open)
        (world/update-entity-component game e2 :open (constantly true))

        :default (dialog/message game "You can't, there's an obstacle.")))

(defmethod move :player [e game x y]
  (let [state (world/current-world-state game)
        game (world/dup-world-state game)
        [x y] (util/matrix-add (:position e) [x y])
        es (world/entities-at state [x y])]
    (if (render/in-bounds? (:foundation state) x y)
      (if (can-move? es)
        (-> game
            (world/update-entity-component e :position (constantly [x y]))
            (viewport/center-viewport e))
        (reduce #(move-into %1 (world/entity %1 (:id e)) %2) game es))
      (dialog/message game "Somehow, you can't move here.."))))
