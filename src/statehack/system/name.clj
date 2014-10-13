(ns statehack.system.name
  (:refer-clojure :exclude [name])
  (:require [statehack.entity :as entity]))

(def race-names
  {:human "Human"})

(defn name [e]
  (cond (entity/capable? e :name) (:name e)
        (entity/capable? e :race) (race-names (:race e))
        :default "WTF"))
