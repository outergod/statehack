(ns statehack.system.world
  (:require [statehack.util :as util]
            [statehack.entity :as entity]))

(def store (atom {}))

(defn save [game]
  (swap! store (constantly (select-keys game [:world])))
  game)

(defn state [game]
  (-> game :world first))

(defn entities [game]
  (:entities (state game)))

(defn entity [game id]
  ((entities game) id))

(defn dup-world-state [game]
  (let [world (:world game)
        state (first world)]
    (assoc game :world (cons state world))))

(defn update-world-state [game f & args]
  (update-in game [:world] (fn [[x & xs]] (cons (apply f x args) xs))))

(defn update-in-world-state [game [& ks] f & args]
  (update-world-state game #(apply update-in % ks f args)))

(defn update-entity [game e f & args]
  (apply update-in-world-state game [:entities (:id e)] f args))

(defn remove-entity-component [game e c & cs]
  (update-in-world-state game [:entities (:id e)] #(apply dissoc % c cs)))

(defn update-entity-component [game e c f & args]
  (let [c (if (sequential? c) c [c])]
    (apply update-in-world-state game (concat [:entities (:id e)] c) f args)))

(defn update-entities [game f & args]
  (apply update-in-world-state game [:entities] f args))

(defn update-entities-component [game c f args]
  (update-entities game #(into {} (map (fn [[k v]] [k (apply update-in c f args)]) %))))

(defn add-entity [game e]
  (update-entities game #(assoc % (:id e) e)))

(defn remove-entity [game e]
  (update-entities game #(dissoc % (:id e))))

(defn pop-world-state [game]
  (update-in game [:world] #(if (> (count %) 1) (next %) %)))

(def neighbors
  (set (remove #(= % [0 0])
               (for [x [-1 0 1] y [-1 0 1]] [x y]))))

(defn entities-at [game floor & coords]
  (let [entities (entities game)
        coords (set coords)]
    (filter #(and (= (:floor %) floor) (coords (:position %)))
            (vals entities))))

(defn direct-neighbors [game [x y] floor]
  (apply entities-at game floor (map (partial util/matrix-add [x y]) neighbors)))

(defn entity-neighbors [game e]
  (direct-neighbors game (:position e) (:floor e)))

(defn capable-entities [game & cs]
  (entity/filter-capable cs (vals (entities game))))

(defn entity-delta [e1 e2]
  (util/matrix-subtract (:position e1) (:position e2)))
