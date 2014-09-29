(ns statehack.system.input
  (:require [statehack.system.input.receivers :as receivers]
            [statehack.game.world :as world]
            [statehack.system.render :as render]
            [statehack.system.viewport :as viewport]
            [statehack.system.movement :as movement]
            [statehack.system.door :as door]
            [lanterna.screen :as screen]))

(def receive-hierarchy (make-hierarchy))

(defn derive-receive [tag parent]
  (alter-var-root #'receive-hierarchy derive tag parent))

(defn- receive-dispatch [game e input]
  (:input e))

(defmulti receive #'receive-dispatch :hierarchy #'receive-hierarchy)

(defn system [{:keys [screen] :as game}]
  (loop [input nil game (render/system game)]
    (print (prn-str input))
    (let [{:keys [receivers entities]} (world/current-world-state game)
          e (entities (first receivers))
          {:keys [quit] :as game} (-> (receive game e input) render/system)]
      (when-not quit (recur (screen/get-key-blocking screen) game)))))

(def player-moves
  {:up [0 -1]
   :down [0 1]
   :left [-1 0]
   :right [1 0]
   :up-left [-1 -1]
   :up-right [1 -1]
   :down-left [-1 1]
   :down-right [1 1]})

(defn action [game player [x y]]
  (let [moves (apply merge (map #(% game player)
                                [door/available-open movement/available-moves]))]
    (if-let [m (moves [x y])]
      (-> game world/dup-world-state m)
      game)))

(defmethod receive :player [game player input]
  (case input
    :up (viewport/update-viewport game [0 -1])
    :down (viewport/update-viewport game [0 1])
    :left (viewport/update-viewport game [-1 0])
    :right (viewport/update-viewport game [1 0])
    \w (action game player [0 -1])
    \s (action game player [0 1])
    \a (action game player [-1 0])
    \d (action game player [1 0])
    \q (action game player [-1 -1])
    \e (action game player [1 -1])
    \z (action game player [-1 1])
    \c (action game player [1 1])
    ; \C (close-next-door game)
    :backspace (-> game world/pop-world-state (viewport/center-viewport player))
    :enter (do
             (swap! world/state (constantly (:world game)))
             game)
    :escape (assoc game :quit true)
    (do (println "unmapped key" input)
        game)))

(defmethod receive :dialog [game dialog input]
  (case input
    (:enter :space) (if (> (count (:messages dialog)) 1)
                      (world/update-entity-component game dialog :messages next)
                      (-> game receivers/pop-control (world/remove-entity dialog)))
    (do (println "unmapped key" input)
        game)))
