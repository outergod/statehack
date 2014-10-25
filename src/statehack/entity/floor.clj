(ns statehack.entity.floor
  (:require [statehack.entity :refer :all]
            [statehack.component :as c]))

(defn floor [n [w h]]
  (entity
   (c/renderable :floor)
   (c/floor n)
   (c/foundation [w h])))
