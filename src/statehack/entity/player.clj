(ns statehack.entity.player
  (:require [statehack.entity :refer :all]
            [statehack.component :refer :all]))

(defn player [x y]
  (entity
   (position x y)
   (mobile :player)
   (renderable :player)
   (input :player)))
