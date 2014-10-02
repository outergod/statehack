(ns statehack.entity.status-bar
  (:require [statehack.entity :refer :all]
            [statehack.component :as c]))

(defn status-bar []
  (entity
   (c/renderable :status)))
