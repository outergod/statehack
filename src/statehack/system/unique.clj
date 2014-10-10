(ns statehack.system.unique
  (:require [statehack.system.world :as world]))
f
(defn unique-entity [game type]
  (let [es (filter #(= (:unique %) type) (vals (world/entities game)))]
    (if (= (count es) 1)
      (first es)
      (throw (ex-info (format "Found %d entities satisfying %s, expected exactly one" (count es) type) {})))))
