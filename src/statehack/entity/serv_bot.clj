(ns statehack.entity.serv-bot
  (:require [statehack.entity :refer :all]
            [statehack.component :as c]))

(defn serv-bot [[x y z]]
  (entity
   (c/position [x y])
   (c/floor z)
   (c/alive true)
   (c/category :serv-bot)
   (c/mobile :wheels)
   (c/renderable :serv-bot)
   (c/obstacle)
   (c/vulnerable 20)
   (c/ai :serv-bot)
   (c/skillset :appendages {:type :melee :strength 8})))
