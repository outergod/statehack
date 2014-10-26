(ns statehack.entity.room
  (:require [statehack.entity :refer :all]
            [statehack.component :as c]))

(defn wall [[x y z]]
  (entity
   (c/renderable :wall)
   (c/position [x y])
   (c/floor z)
   (c/room)
   (c/obstacle)
   (c/opaque)))

(defn solid [[x y z]]
  (entity
   (c/renderable {:tile :nihil :color 0})
   (c/position [x y])
   (c/floor z)
   (c/obstacle)
   (c/opaque)))

(defn door [[x y z] open?]
  (entity
   (c/renderable :door)
   (c/position [x y])
   (c/floor z)
   (c/door open?)
   (c/room)
   (c/obstacle :door)
   (c/opaque :door)))
