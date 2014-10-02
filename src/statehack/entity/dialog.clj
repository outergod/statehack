(ns statehack.entity.dialog
  (:require [statehack.entity :refer :all]
            [statehack.component :as c]))

(defn dialog [ms]
  (entity
   (c/renderable :dialog)
   (c/input :dialog)
   (c/messages ms)))
