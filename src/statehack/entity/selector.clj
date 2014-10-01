(ns statehack.entity.selector
  (:require [statehack.entity :refer :all]
            [statehack.component :as c]))

(defn selector [targets]
  (entity
   (c/position 0 0)
   (c/mobile :selector :targets targets)
   (c/input :selector)))
