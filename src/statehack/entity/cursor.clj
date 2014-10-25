(ns statehack.entity.cursor
  (:require [statehack.entity :refer :all]
            [statehack.component :as c]))

(defn cursor []
  (entity
   (c/unique :cursor)
   (c/position [0 0])
   (c/mobile :cursor)))
