(ns statehack.system.transition
  (:require [statehack.system.sound :as sound]
            [statehack.system.render :as render]))

(defn sound [name]
  #(future (sound/play-sound name)))

(defn transition [game f]
  (update-in game [:transition] conj f))

(defn system [game]
  (doseq [t (:transition game)]
    (t))
  (assoc game :transition []))
