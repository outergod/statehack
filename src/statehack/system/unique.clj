(ns statehack.system.unique
  (:require [statehack.system.world :as world]))

(defn unique-entity [game type]
  (let [es (filter #(= (:unique %) type) (vals (world/entities game)))]
    (case (count es)
      0 nil
      1 (first es)
      (throw (ex-info (format "Found %d entities satisfying %s, expected exactly one" (count es) type) {})))))
