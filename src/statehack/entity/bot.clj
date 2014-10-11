(ns statehack.entity.bot
  (:require [statehack.entity :refer :all]
            [statehack.component :as c]))

(defn bot [x y hp]
  (entity
   (c/alive true)
   (c/position x y)
   (c/race :human)
   (c/mobile :humanoid)
   (c/renderable :humanoid)
   (c/obstacle)
   (c/vulnerable hp)
   (c/ai :hostile)))
