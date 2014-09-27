(ns statehack.system.movement
  (:require [statehack.system.render :as render]
            [statehack.system.viewport :as viewport]
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
      (if-let [os (seq (filter #(= (:position %) [x y]) (vals (dissoc (:entities state) (:id e)))))]
        game #_(let [[game e object] (entity/collide game e (first os))]
          (-> game world/dup-world-state
              (world/update-world-state
               #(-> %
                    (assoc-in [:entities (:id e)] e)
                    (assoc-in [:entities (:id object)] object)))))
        (-> game world/dup-world-state
            (world/update-world-state #(update-in % [:entities (:id e)] update-position x y))
            (viewport/center-viewport e)))
      (world/message game "Somehow, you can't move here.."))))
