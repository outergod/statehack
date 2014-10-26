(ns statehack.system.name
  (:refer-clojure :exclude [name])
  (:require [statehack.entity :as entity]))

(def category-names
  {:human "Human"})

(defn name [e]
  (cond (entity/capable? e :name) (:name e)
        (entity/capable? e :category) (category-names (:category e))
        :default "WTF"))
