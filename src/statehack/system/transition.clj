(ns statehack.system.transition
  (:require [statehack.system.sound :as sound]
            [statehack.system.render :as render]))

(defn punch []
  (sound/play :punch-02)
  (Thread/sleep 200))

(defn transition [game f]
  (assoc game :transition f))

(defn system [game]
  (when-let [t (:transition game)]
    (t))
  (dissoc game :transition))
