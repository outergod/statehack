(ns statehack.system.memory
  "Memory system"
  (:require [statehack.system.levels :as levels]
            [statehack.system.sight :as sight]
            [statehack.system.world :as world]
            [statehack.util :as util]
            [clojure.set :as set]))

(defn memory-floor [e n]
  (get-in e [:memory :floors n] {}))

(defn update-memory-floor [game e n es mask]
  (world/update-entity-component game e :memory
                                 #(-> %
                                      (update-in [:floors n :entities] merge es)
                                      (update-in [:floors n :coordinates] set/union mask))))

(defn remember-view [game e]
  {:pre [(:sight e) (:memory e) (:floor e)]}
  (let [{:keys [floor]} (levels/entity-floor game e)
        mask (sight/visible-mask game e)
        es (dissoc (util/index-by :id (sight/visible-entities game floor mask)) (e :id))]
    (update-memory-floor game e floor es mask)))

(defn system [game]
  (let [es (world/capable-entities game :memory :sight :floor)]
    (reduce remember-view game es)))
