(ns statehack.game
  (:require [statehack.game.world :as world]
            [statehack.game.dialog :as dialog]
            [statehack.ui :as ui]
            [statehack.util :as util]
            [statehack.entity :as entity]
            [statehack.ui.room :as room]
            [lanterna.screen :as screen]))

(defn update-viewport [game [x y]]
  (let [state (-> game :world first)
        scr (:screen game)]
    (update-in game [:viewport] #(ui/into-bounds state scr (util/matrix-add % [x y])))))

(defn set-viewport [game [x y]]
  (let [state (-> game :world first)
        foundation (:foundation state)
        scr (:screen game)]
    (update-in game [:viewport] (constantly (ui/into-bounds foundation scr [x y])))))

(defn center-viewport-player [game]
  (let [scr (:screen game)]
    (set-viewport game (ui/center scr (-> game :world first :player :position)))))

(defn move-player [player x y]
  (assoc-in player [:position] [x y]))

(defn in-bounds? [canvas x y]
  (and (>= x 0) (>= y 0)
       (< x (count (first canvas))) (< y (count canvas))))

(defn move-into [game x y]
  (let [state (first (:world game))
        [x y] (util/matrix-add (get-in state [:player :position]) [x y])]
    (if (in-bounds? (:foundation state) x y)
      (if-let [os (seq (filter #(= (:position %) [x y]) (vals (:entities state))))]
        (let [[game player object] (entity/collide game (:player state) (first os))]
          (world/push-world-state game #(-> %
                                            (assoc :player player)
                                            (assoc-in [:entities (:id object)] object))))
        (-> game
            (world/update-world-state [:player] move-player x y)
            center-viewport-player))
      (dialog/message game "Somehow, you can't move here.."))))

(def first-room
"XXXXXoXXXXX
X         X
X         X
X         X
X         X
X         X
XXXXXXXXXXX")

(defn new-game [scr]
  {:screen scr
   :viewport [0 0]
   :world [{:mode :world
            :foundation (ui/space 80 24)
            :player (entity/player 40 18)
            :entities (util/index-by :id 
                        (flatten
                         [(room/extract-room first-room 35 13)]))}]})

(defn load-game [scr world]
  {:screen scr
   :viewport [0 0]
   :world world})

(defn close-next-door [game]
  (let [state (world/current-world-state game)
        player (:player state)
        [x y] (:position player)]
    (if-let [candidates (room/close-candidates game x y)]
      (let [[game player door] (room/toggle-door game player (first candidates) false)]
        (world/push-world-state game #(-> %
                                          (assoc :player player)
                                          (assoc-in [:entities (:id door)] door))))
      (dialog/message game "No open door nearby"))))

(defmethod world/transition :world [game input]
  (case input
    :up (update-viewport game [0 -1])
    :down (update-viewport game [0 1])
    :left (update-viewport game [-1 0])
    :right (update-viewport game [1 0])
    \w (move-into game 0 -1)
    \s (move-into game 0 1)
    \a (move-into game -1 0)
    \d (move-into game 1 0)
    \q (move-into game -1 -1)
    \e (move-into game 1 -1)
    \z (move-into game -1 1)
    \c (move-into game 1 1)
    \C (close-next-door game)
    :backspace (-> game world/pop-world-state center-viewport-player)
    :enter (do
             (swap! world/state (constantly (:world game)))
             game)
    (do (println "unmapped key" input)
        game)))

(defn run
  ([scr game]
     (doall (take-while identity (repeatedly #(screen/get-key scr))))
     (screen/in-screen scr
       (loop [input nil game game]
         (print (prn-str input))
         (when (not= input :escape)
           (let [game (world/transition game input)]
             (ui/drawing scr (ui/draw-game game))
             (recur (screen/get-key-blocking scr) game))))))
  ([scr]
     (run scr (new-game scr))))
