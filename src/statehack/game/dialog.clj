(ns statehack.game.dialog
  (:require [statehack.input :as input]
            [statehack.game.world :as world]))

(defmethod input/input-system :dialog [game input]
  (case input
    (:enter :space) (-> game world/dup-world-state
                        (world/update-world-state #(let [ms (:messages %)]
                                                     (if (> (count ms) 1)
                                                       (assoc % :messages (next ms))
                                                       (-> % (dissoc :messages) (assoc :mode :world))))))
    (do (println "unmapped key" input)
        game)))

(defn messages [game ms]
  {:pre [(coll? ms)]}
  (-> game world/dup-world-state
      (world/update-world-state #(-> % (assoc :mode :dialog) (assoc :messages ms)))))

(defn message [game s]
  (messages game [s]))
