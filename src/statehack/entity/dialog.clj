(ns statehack.entity.dialog
  (:require [statehack.entity :refer :all]
            [statehack.component :as c]))

(defn dialog [& ms]
  (entity
   (c/unique :dialog)
   (c/renderable :dialog)
   (c/input :dialog)
   (apply c/messages ms)))
