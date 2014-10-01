(ns statehack.system.input
  (:require [statehack.system.input.receivers :as receivers]
            [statehack.game.world :as world]
            [statehack.system.render :as render]
            [statehack.system.viewport :as viewport]
            [statehack.system.movement :as movement]
            [statehack.system.door :as door]
            [statehack.system.defer :as defer]
            [lanterna.screen :as screen]))

(def receive-hierarchy (make-hierarchy))

(defn derive-receive [tag parent]
  (alter-var-root #'receive-hierarchy derive tag parent))

(defn- receive-dispatch [game e input]
  (:input e))

(defmulti receive #'receive-dispatch :hierarchy #'receive-hierarchy)

(defn receiver [state]
  (let [{:keys [receivers entities]} state]
    (entities (first receivers))))

(defn system [{:keys [screen] :as game}]
  (loop [input nil game (render/system game)]
    (print (prn-str input))
    (let [e (receiver (world/current-world-state game))
          {:keys [quit] :as game} (-> (receive game e input) movement/system render/system)]
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

(defn action [game player dir]
  (let [[x y] (player-moves dir)
        moves (apply merge (map #(% game player)
                                (reverse [door/available-open movement/available-moves])))
        non-move ((movement/unavailable-moves game player) [x y])]
    (if-let [m (moves [x y])]
      (-> game world/dup-world-state m)
      (non-move game))))

(defn viewport [game dir]
  (viewport/update-viewport game (player-moves dir)))

(defmethod receive :player [game player input]
  (case input
    :up (viewport game :up)
    :down (viewport game :down)
    :left (viewport game :left)
    :right (viewport game :right)
    \w (action game player :up)
    \s (action game player :down)
    \a (action game player :left)
    \d (action game player :right)
    \q (action game player :up-left)
    \e (action game player :up-right)
    \z (action game player :down-left)
    \c (action game player :down-right)
    \C (door/close game player)
    :backspace (-> game world/pop-world-state (viewport/center-viewport player))
    :enter (do
             (swap! world/state (constantly (:world game)))
             game)
    :escape (assoc game :quit true)
    (do (println "unmapped key" input)
        game)))

(defmethod receive :selector [game selector input]
  (case input
    :up (viewport game :up)
    :down (viewport game :down)
    :left (viewport game :left)
    :right (viewport game :right)
    :tab (movement/move-next game selector)
    (:enter :space) (defer/fulfill game selector)
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
