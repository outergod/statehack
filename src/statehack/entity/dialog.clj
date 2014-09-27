(ns statehack.entity.dialog
  (:require [statehack.entity :refer :all]
            [statehack.component :refer :all]))

(defn dialog [ms]
  (entity
   (renderable :dialog)
   (input :dialog)
   (messages ms)))
