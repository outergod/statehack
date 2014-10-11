(ns statehack.entity.log
  (:require [statehack.entity :refer :all]
            [statehack.component :as c]))

(defn log []
  (entity
   (c/unique :log)
   (c/renderable :log)
   (c/messages)))
