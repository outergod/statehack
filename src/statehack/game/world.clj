(ns statehack.game.world
  (:require [statehack.util :as util]))

(def state (atom {}))

(defn current-world-state [game]
  (-> game :world first))

(defn player-entity [game]
  (let [s (current-world-state game)]
    ((:entities s) (:player s))))

(defn entity [game id]
  (let [s (current-world-state game)]
    ((:entities s) id)))

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

(defn update-entity-component [game e c f & args]
  (apply update-in-world-state game [:entities (:id e) c] f args))

(defn update-entities [game f & args]
  (apply update-in-world-state game [:entities] f args))

(defn update-entities-component [game c f args]
  (update-entities game #(into {} (map (fn [[k v]] [k (apply update-in c f args)]) %))))

(defn add-entity [game e]
  (update-entities game #(assoc % (:id e) e)))

(defn remove-entity [game e]
  (update-entities game #(dissoc % (:id e))))

#_(defn pop-world-state [game]
  (update-in game [:world]
             (fn [ss]
               (loop [ss ss init true]
                 (let [{:keys [mode]} (first ss)]
                   (if (and (> (count ss) 1)
                            (or init (not= mode :world)))
                     (recur (next ss) false)
                     ss))))))

(defn pop-world-state [game]
  (update-in game [:world] #(if (> (count %) 1) (next %) %)))

(def neighbors
  (set (remove #(= % [0 0])
               (for [x [-1 0 1] y [-1 0 1]] [x y]))))

(defn entities-at [state & coords]
  (let [{:keys [entities]} state
        coords (set coords)]
    (filter #(coords (:position %)) (vals entities))))

(defn direct-neighbors [state x y]
  (apply entities-at state (map (partial util/matrix-add [x y]) neighbors)))

(defn entity-neighbors [e game]
  (let [state (current-world-state game)
        [x y] (:position e)]
    (direct-neighbors state x y)))

(defn entity-delta [e1 e2]
  (util/matrix-subtract (:position e1) (:position e2)))
