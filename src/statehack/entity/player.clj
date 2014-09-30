(ns statehack.entity.player
  (:require [statehack.entity :refer :all]
            [statehack.component :as c]))

(defn player [x y]
  (entity
   (c/position x y)
   (c/mobile :humanoid)
   (c/renderable :humanoid)
   (c/input :player)
   (c/obstacle)))
