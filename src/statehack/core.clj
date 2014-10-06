(ns statehack.core
  (:gen-class)
  (:require [statehack.game :as game]
            [halo.screen :as screen]))

(defn -main
  [& args]
  (game/run (screen/screen)))
