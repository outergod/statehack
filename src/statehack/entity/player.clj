(ns statehack.entity.player
  (:require [statehack.entity :refer :all]
            [statehack.component :as c]))

(defn player [name x y hp]
  (entity
   (c/player)
   (c/named name)
   (c/adaptive 0 0)
   (c/position x y)
   (c/mobile :humanoid)
   (c/renderable :humanoid)
   (c/input :player)
   (c/obstacle)
   (c/vulnerable hp)))
