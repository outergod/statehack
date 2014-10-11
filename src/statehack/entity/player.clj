(ns statehack.entity.player
  (:require [statehack.entity :refer :all]
            [statehack.component :as c]))

(defn player [name x y hp]
  (entity
   (c/unique :player)
   (c/alive true)
   (c/position x y)
   (c/named name)
   (c/race :human)
   (c/adaptive 0 0)
   (c/skillset {:martial-arts 1})
   (c/mobile :humanoid)
   (c/renderable :humanoid)
   (c/input :player)
   (c/obstacle)
   (c/vulnerable hp)))
