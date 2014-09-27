(ns statehack.game.world
  (:require [statehack.util :as util]
            [statehack.entity.dialog :as dialog]))

(def state (atom {}))

(defn current-world-state [game]
  (-> game :world first))

(defn player-entity [game]
  (let [s (current-world-state game)]
    ((:entities s) (:player s))))

(defn dup-world-state [game]
  (let [world (:world game)
        state (first world)]
    (assoc game :world (cons state world))))

(defn update-world-state [game f]
  (update-in game [:world] (fn [[x & xs]] (cons (f x) xs))))

(defn update-entity [game e f]
  (update-world-state game #(update-in % [:entities (:id e)] f)))

(defn update-entity-component [game e c f]
  (update-world-state game #(update-in % [:entities (:id e) c] f)))

(defn add-entity [game e]
  (update-world-state game #(update-in % [:entities]
                                       (fn [es] (assoc es (:id e) e)))))

(defn remove-entity [game e]
  (update-world-state game #(update-in % [:entities]
                                       (fn [es] (dissoc es (:id e))))))

(defn pop-world-state [game]
  (update-in game [:world]
             (fn [ss]
               (loop [ss ss init true]
                 (let [{:keys [mode]} (first ss)]
                   (if (and (> (count ss) 1)
                            (or init (not= mode :world)))
                     (recur (next ss) false)
                     ss))))))

(def neighbors
  (set (remove #(= % [0 0])
               (apply concat
                      (for [y [-1 0 1]]
                        (for [x [-1 0 1]]
                          [x y]))))))

(defn entities-at [state coords]
  (let [{:keys [entities]} state
        coords (set coords)]
    (filter #(coords (:position %)) (vals entities))))

(defn direct-neighbors [state x y]
  (entities-at state (map (partial util/matrix-add [x y]) neighbors)))

(defn messages [game ms]
  {:pre [(coll? ms)]}
  (add-entity game (dialog/dialog ms)))

(defn message [game s]
  (messages game [s]))
