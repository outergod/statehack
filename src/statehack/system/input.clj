(ns statehack.system.input
  (:require [statehack.system.input.receivers :as receivers]
            [statehack.game.world :as world]
            [statehack.system.render :as render]
            [statehack.system.viewport :as viewport]
            [statehack.system.movement :as movement]
            [lanterna.screen :as screen]))

(def receive-hierarchy (make-hierarchy))

(defn derive-receive [tag parent]
  (alter-var-root #'receive-hierarchy derive tag parent))

(defn- receive-dispatch [e game input]
  (:input e))

(defmulti receive #'receive-dispatch :hierarchy #'receive-hierarchy)

(defn system [{:keys [screen] :as game}]
  (loop [input nil game (render/system game)]
    (print (prn-str input))
    (let [{:keys [receivers entities]} (world/current-world-state game)
          e (entities (first receivers))
          {:keys [quit] :as game} (-> (receive e game input) render/system)]
      (when-not quit (recur (screen/get-key-blocking screen) game)))))



(defmethod receive :player [player game input]
  (case input
    :up (viewport/update-viewport game [0 -1])
    :down (viewport/update-viewport game [0 1])
    :left (viewport/update-viewport game [-1 0])
    :right (viewport/update-viewport game [1 0])
    \w (movement/move player game 0 -1)
    \s (movement/move player game 0 1)
    \a (movement/move player game -1 0)
    \d (movement/move player game 1 0)
    \q (movement/move player game -1 -1)
    \e (movement/move player game 1 -1)
    \z (movement/move player game -1 1)
    \c (movement/move player game 1 1)
    ; \C (close-next-door game)
    :backspace (-> game world/pop-world-state (viewport/center-viewport player))
    :enter (do
             (swap! world/state (constantly (:world game)))
             game)
    :escape (assoc game :quit true)
    (do (println "unmapped key" input)
        game)))

(defmethod receive :dialog [dialog game input]
  (case input
    (:enter :space) (if (> (count (:messages dialog)) 1)
                      (world/update-entity-component game dialog :messages next)
                      (-> game receivers/pop-control (world/remove-entity dialog)))
    (do (println "unmapped key" input)
        game)))
