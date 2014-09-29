(ns statehack.system.movement
  (:require [statehack.entity :as entity]
            [statehack.system.render :as render]
            [statehack.system.viewport :as viewport]
            [statehack.system.dialog :as dialog]
            [statehack.game.world :as world]
            [statehack.util :as util]
            [clojure.set :as set]))

(def move-hierarchy (make-hierarchy))

(defn- available-moves-dispatch [game e]
  (:mobile e))

(defmulti available-moves #'available-moves-dispatch :hierarchy #'move-hierarchy)
(defmethod available-moves :default [& _] nil)

(defmethod available-moves :humanoid [game e]
  (let [es (remove :open (world/entity-neighbors game e))
        ds (set (map #(world/entity-delta % e) es))]
    (into {} (map (fn [pos] [pos #(move % e pos)])
                  (set/difference world/neighbors ds)))))

(defn move [game e [x y]]
  (let [f (:foundation (world/current-world-state game))
        [x y] (util/matrix-add (:position e) [x y])]
   (if (render/in-bounds? f [x y])
     (world/update-entity-component game e :position (constantly [x y]))
     (dialog/message game "Somehow, you can't move here.."))))
