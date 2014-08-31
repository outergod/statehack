(ns statehack.game.dialog
  (:require [statehack.game.world :as world]))

(defmethod world/transition :dialog [game input]
  (case input
    (:enter :space) (if (> (count (:messages (world/current-world-state game))) 1)
                      (world/update-world-state game [:messages] next)
                      (world/push-world-state game #(-> % (dissoc :messages) (assoc :mode :world))))
    (do (println "unmapped key" input)
        game)))

(defn messages [game ms]
  {:pre [(coll? ms)]}
  (world/push-world-state game #(-> % (assoc :mode :dialog) (assoc :messages ms))))

(defn message [game s]
  (messages game [s]))
