(ns statehack.system.memory
  "Memory system"
  (:require [clojure.walk :as walk]
            [statehack.system.levels :as levels]
            [statehack.system.render :as render]
            [statehack.system.world :as world]))

(defn memory-floor [e n]
  (get-in e [:memory :floors n]))

(defn update-memory-floor [game e n v]
  (world/update-entity-component game e :memory #(assoc-in % [:floors n] v)))

(defn dye-gray [canvas]
  (walk/postwalk #(if (map? %) (assoc % :color 8) %) canvas))

(defn remember-view [game e]
  {:pre [(:sight e) (:memory e) (:floor e)]}
  (let [{:keys [floor foundation]} (levels/entity-floor game e)
        memory (or (memory-floor e floor)
                   (render/rect :nihil 0 foundation))
        updated (dye-gray (render/canvas-blit memory (render/visible-world game e) [0 0]))]
    (update-memory-floor game e floor updated)))

(defn system [game]
  (let [es (world/capable-entities game :memory :sight :floor)]
    (reduce remember-view game es)))
