(ns statehack.entity.bot
  (:require [statehack.entity :refer :all]
            [statehack.component :as c]))

(defn bot [x y]
  (entity
   (c/position x y)
   (c/mobile :humanoid)
   (c/renderable :humanoid)
   (c/input :ai)
   (c/obstacle)))
