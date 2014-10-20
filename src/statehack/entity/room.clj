(ns statehack.entity.room
  (:require [statehack.entity :refer :all]
            [statehack.component :as c]))

(defn wall [x y]
  (entity
   (c/renderable :wall)
   (c/position x y)
   (c/room)
   (c/obstacle)))

(defn solid [x y]
  (entity
   (c/renderable {:tile :nihil :color 0})
   (c/position x y)
   (c/obstacle)))

(defn door [x y open?]
  (entity
   (c/renderable :door)
   (c/position x y)
   (c/door open?)
   (c/room)
   (c/obstacle :door)))
