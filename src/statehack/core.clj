(ns statehack.core
  (:gen-class)
  (:require [statehack.game :as game]
            [lanterna.screen :as screen]))

(defn -main
  [& args]
  (game/run (case (keyword (first args))
              :gui  (screen/get-screen :swing {:cols 80 :rows 24 :font "Inconsolata" :font-size 16})
              :text (screen/get-screen :text))))
