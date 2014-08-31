(ns statehack.core
  (:gen-class)
  (:require [statehack.ui :as ui]
            [statehack.util :as util]
            [statehack.entity :as entity]
            [statehack.ui.room :as room]
            [lanterna.screen :as screen]))

(def state (atom {}))

(defn update-viewport [game [x y]]
  (let [state (-> game :world first)
        scr (:screen game)]
    (update-in game [:viewport] #(ui/into-bounds state scr (util/matrix-add % [x y])))))

(defn set-viewport [game [x y]]
  (let [state (-> game :world first)
        foundation (:foundation state)
        scr (:screen game)]
    (update-in game [:viewport] (constantly (ui/into-bounds foundation scr [x y])))))

(defn push-world-state [game f]
  (let [world (:world game)
        state (first world)]
    (assoc game :world (cons (f state) world))))

(defn update-world-state [game & args]
  (push-world-state game #(apply update-in % args)))

(defn pop-world-state [game]
  (update-in game [:world] #(if (> (count %) 1) (next %) %)))

(defn center-viewport-player [game]
  (let [scr (:screen game)]
    (set-viewport game (ui/center scr (-> game :world first :player :position)))))

(defn move-player [player x y]
  (assoc-in player [:position] [x y]))

(defn move-into [game x y]
  (let [state (first (:world game))
        [x y] (util/matrix-add (get-in state [:player :position]) [x y])
        scr (:screen game)]
    (if-let [os (seq (filter #(= (:position %) [x y]) (:entities state)))]
      (let [[game _ _] (entity/collide game (:player state) (first os))]
        game)
      (-> game
          (update-world-state [:player] move-player x y)
          center-viewport-player))))

(defn transition [game input]
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
    :backspace (-> game pop-world-state center-viewport-player)
    :enter (do
             (swap! state (constantly (:world game)))
             game)
    game))

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
   :world [{:foundation (ui/space 80 24)
            :player (entity/player 40 18)
            :entities (flatten
                       [(room/extract-room first-room 35 13)])}]})

(defn load-game [scr world]
  {:screen scr
   :viewport [0 0]
   :world world})

(defn run
  ([scr game]
     (screen/in-screen scr
       (loop [input nil game game]
         (print (prn-str input))
         (when (not= input :escape)
           (let [game (transition game input)]
             (ui/drawing scr (ui/draw-game scr game))
             (recur (screen/get-key-blocking scr) game))))))
  ([scr]
     (run scr (new-game scr))))

(defn -main
  [& args]
  (run (case (keyword (first args))
         :gui  (screen/get-screen :swing {:cols 80 :rows 24 :font "Inconsolata" :font-size 16})
         :text (screen/get-screen :text))))
