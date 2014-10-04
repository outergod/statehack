(ns user
  (:refer-clojure)
  (:require [lanterna.screen :as screen]
            [statehack.system :as system]
            [statehack.game :as game]
            [statehack.entity :as entity]
            [statehack.system.world :as world]
            [statehack.system.input :as input]
            [statehack.system.movement :as movement]
            [statehack.system.door :as door]
            [statehack.system.sound :as sound]
            [statehack.util :as util]
            [clojure.stacktrace :refer :all]
            [clojure.pprint :refer :all])
  (:import [clojure.lang IExceptionInfo]))

(def scr (screen/get-screen :text))

(def crash-state (atom {}))

(defn run []
  (try (game/run scr)
       (catch Throwable e
         (if (instance? IExceptionInfo e)
           (let [{:keys [state]} (ex-data e)]
             (println "Crash state available.")
             (swap! crash-state (constantly state))
             (throw (.getCause e)))
           (throw e)))))

(comment
  (game/run scr)

  (let [game (game/load-game scr @world/state)
        player (world/player-entity game)]
    (movement/available-moves game player)))
