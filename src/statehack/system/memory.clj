(ns statehack.system.memory
  "Memory system"
  (:require [statehack.system.levels :as levels]
            [statehack.system.sight :as sight]
            [statehack.system.world :as world]
            [statehack.util :as util]
            [clojure.set :as set]))

(defn entity-memory
  [e] (:memory e))

(defn update-memory [game e f & args]
  (apply world/update-entity-component game e :memory f args))

(defn entity-floor-memory
  ([e n] (get-in e [:memory :floors n] {:entities {} :coordinates []}))
  ([e] (entity-floor-memory e (:floor e))))

(defn update-memory-floor
  ([game e n f]
     (update-memory game e #(update-in % [:floors n] f)))
  ([game e f]
     (update-memory-floor game e (:floor (levels/entity-floor game e)) f)))

(defn remember-view [game e]
  {:pre [(:sight e) (:memory e) (:floor e)]}
  (let [{:keys [floor]} (levels/entity-floor game e)
        mask (sight/visible-mask game e)
        es (dissoc (util/index-by :id (sight/visible-entities game floor mask)) (e :id))]
    (update-memory-floor game e floor
                         #(-> %
                              (update-in [:entities] merge es)
                              (update-in [:coordinates] set/union mask)))))

(defn system [game]
  (let [es (world/capable-entities game :memory :sight :floor)]
    (reduce remember-view game es)))
