(ns statehack.entity.selector
  (:require [statehack.entity :refer :all]
            [statehack.component :as c]))

(defn selector [[x y] action targets]
  (entity
   (c/position [x y])
   (c/mobile :selector :targets targets)
   (c/input :selector)
   (c/deferred action)))
