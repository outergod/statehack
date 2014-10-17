(ns statehack.system.transition
  (:require [statehack.system.sound :as sound]
            [statehack.system.render :as render]))

(defn door []
  (future (sound/play :door)))

(defn die []
  (future (sound/play :man-dying)))

(defn punch []
  (future (sound/play :punch-02)))

(defn transition [game f]
  (update-in game [:transition] conj f))

(defn system [game]
  (doseq [t (:transition game)]
    (t))
  (assoc game :transition []))
