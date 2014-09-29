(ns statehack.entity.player
  (:require [statehack.entity :refer :all]
            [statehack.component :as c]))

(defn player [x y]
  (entity
   (c/position x y)
   (c/mobile :humanoid)
   (c/renderable :player)
   (c/input :player)
   (c/obstacle)))
