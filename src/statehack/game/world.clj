(ns statehack.game.world
  (:require [statehack.util :as util]))

(def state (atom {}))

(defn current-world-state [game]
  (-> game :world first))

(defn dup-world-state [game]
  (let [world (:world game)
        state (first world)]
    (assoc game :world (cons state world))))

#_(defn push-world-state [game f]
  (let [world (:world game)
        state (first world)]
    (assoc game :world (cons (f state) world))))

#_(defn update-world-state [game & args]
  (push-world-state game #(apply update-in % args)))

(defn update-world-state [game f]
  (update-in game [:world] #(cons (f (first %)) %)))

(defn pop-world-state [game]
  (update-in game [:world] #(if (> (count %) 1) (next %) %)))

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
