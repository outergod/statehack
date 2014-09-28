(ns statehack.system.movement
  (:require [statehack.system.render :as render]
            [statehack.system.viewport :as viewport]
            [statehack.system.dialog :as dialog]
            [statehack.game.world :as world]
            [statehack.util :as util]))

(def move-hierarchy (make-hierarchy))

(defn- move-dispatch [e game x y]
  (:mobile e))

(defmulti move #'move-dispatch :hierarchy #'move-hierarchy)

(defn update-position [e x y]
  (assoc-in e [:position] [x y]))

(defmethod move :player [e game x y]
  (let [state (world/current-world-state game)
        [x y] (util/matrix-add (:position e) [x y])]
    (if (render/in-bounds? (:foundation state) x y)
      (if (seq (world/entities-at state [x y]))
        (dialog/message game "You can't, there's an obstacle.")
        (-> game world/dup-world-state
            (world/update-entity e update-position x y)
            (viewport/center-viewport e)))
      (dialog/message game "Somehow, you can't move here.."))))
