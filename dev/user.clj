(ns user
  (:require [halo.screen :as screen]
            [halo.graphics :as graphics]
            [statehack.system :as system]
            [statehack.game :as game]
            [statehack.entity :as entity]
            [statehack.system.world :as world]
            [statehack.system.input :as input]
            [statehack.system.movement :as movement]
            [statehack.system.door :as door]
            [statehack.system.sound :as sound]
            [statehack.system.render :as render]
            [statehack.util :as util]
            [clojure.stacktrace :refer :all]
            [clojure.pprint :refer :all]
            [clojure.reflect :as reflect])
  (:import [clojure.lang IExceptionInfo]))

(defn publics [o]
  (sort (set (map :name (filter #(:public (:flags %))
                                (:members (reflect/reflect o :ancestors true)))))))

(def screen (screen/screen))

(def crash-state (atom {}))

(defn run []
  (try (game/run screen)
       (catch Throwable e
         (if (instance? IExceptionInfo e)
           (let [{:keys [state]} (ex-data e)]
             (println "Crash state available.")
             (swap! crash-state (constantly state))
             (throw (.getCause e)))
           (throw e)))))

(comment
  (let [game (game/load-game screen @world/state)
        player (world/player-entity game)]
    (movement/available-moves game player)))
