(ns statehack.game.world
  (:require [statehack.util :as util]))

(def state (atom {}))

(defn current-world-state [game]
  (-> game :world first))

(defn dup-world-state [game]
  (let [world (:world game)
        state (first world)]
    (assoc game :world (cons state world))))

(defn update-world-state [game f]
  (update-in game [:world] (fn [[x & xs]] (cons (f x) xs))))

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
