(ns user
  (:require [halo.screen :as screen]
            [halo.graphics :as graphics]
            [statehack.game :as game]
            [statehack.entity :as entity]
            [statehack.system.world :as world]
            [statehack.system.input :as input]
            [statehack.system.movement :as movement]
            [statehack.system.door :as door]
            [statehack.system.sound :as sound]
            [statehack.system.render :as render]
            [statehack.system.unique :as unique]
            [statehack.system.sight :as sight]
            [statehack.algebra :as algebra]
            [statehack.system.levels :as levels]
            [statehack.system.transition :as transition]
            [statehack.util :as util]
            [clj-audio.core :as audio]
            [clj-audio.sampled :as sampled]
            [clojure.stacktrace :refer :all]
            [clojure.pprint :refer :all]
            [clojure.reflect :as reflect]
            [clojure.java.io :as io])
  (:import [clojure.lang IExceptionInfo]))

(defn publics [o]
  (sort (set (map :name (filter #(:public (:flags %))
                                (:members (reflect/reflect o :ancestors true)))))))

(def screen (screen/screen))
(def graphics (screen/text-graphics screen))

(def crash-state (atom {}))

(defn run []
  (try (game/run screen)
       (catch Throwable e
         (if (instance? IExceptionInfo e)
           (let [{:keys [state]} (ex-data e)]
             (println "Crash state available.")
             (swap! crash-state (constantly state))
             (throw (.getCause e)))
           (throw e)))
       (finally (sound/cleanup))))

(comment
  (let [game (game/load-game screen @world/state)
        player (world/player-entity game)]
    (movement/available-moves game player)))
