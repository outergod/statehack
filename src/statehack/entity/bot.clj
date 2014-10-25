(ns statehack.entity.bot
  (:require [statehack.entity :refer :all]
            [statehack.component :as c]))

(defn bot [[x y z] hp]
  (entity
   (c/position [x y])
   (c/floor z)
   (c/alive true)
   (c/race :human)
   (c/mobile :humanoid)
   (c/renderable :humanoid)
   (c/obstacle)
   (c/vulnerable hp)
   (c/ai :hostile)))
