(ns statehack.entity.cursor
  (:require [statehack.entity :refer :all]
            [statehack.component :as c]))

(defn cursor [x y mode]
  (entity
   (c/position x y)
   (c/cursor mode)
   (c/input :cursor)))
