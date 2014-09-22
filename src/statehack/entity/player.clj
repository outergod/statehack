(ns statehack.entity.player
  (:require [statehack.entity :as entity]
            #_[statehack.entity.room :as room]
            [statehack.component.input :as input]
            [statehack.component.render :as render]
            [statehack.game.world :as world]
            [statehack.game.dialog :as dialog]
            [statehack.util :as util]))

(defn player [x y]
  (-> (entity/entity) (entity/position x y) (render/renderable :player) (input/receiver :player)))

(defmethod render/render :player [_] :player)

(defn state-player [state]
  (let [{:keys [entities]} state]
    (-> state :player entities)))

(defn update-viewport [game [x y]]
  (let [state (world/current-world-state game)
        scr (:screen game)]
    (update-in game [:viewport] #(render/into-bounds state scr (util/matrix-add % [x y])))))

(defn set-viewport [game [x y]]
  (let [state (world/current-world-state game)
        foundation (:foundation state)
        scr (:screen game)]
    (update-in game [:viewport] (constantly (render/into-bounds foundation scr [x y])))))

(defn center-viewport-player [game]
  (let [player (state-player (world/current-world-state game))
        scr (:screen game)]
    (set-viewport game (render/center scr (player :position)))))

(defn move-player [player x y]
  (assoc-in player [:position] [x y]))

(defn in-bounds? [canvas x y]
  (and (>= x 0) (>= y 0)
       (< x (count (first canvas))) (< y (count canvas))))

(defn move-into [game x y]
  (let [state (world/current-world-state game)
        player (state-player state)
        [x y] (util/matrix-add (:position player) [x y])]
    (if (in-bounds? (:foundation state) x y)
      (if-let [os (seq (filter #(= (:position %) [x y]) (vals (dissoc (:entities state) (:id player)))))]
        game #_(let [[game player object] (entity/collide game player (first os))]
          (-> game world/dup-world-state
              (world/update-world-state
               #(-> %
                    (assoc-in [:entities (:id player)] player)
                    (assoc-in [:entities (:id object)] object)))))
        (-> game world/dup-world-state
            (world/update-world-state #(update-in % [:entities (:id player)] move-player x y))
            center-viewport-player))
      (dialog/message game "Somehow, you can't move here.."))))

#_(defn close-next-door [game]
  (let [state (world/current-world-state game)
        player (state-player state)
        [x y] (:position player)]
    (if-let [candidates (room/close-candidates game x y)]
      (let [[game player door] (room/toggle-door game player (first candidates) false)]
        (-> game world/dup-world-state
            (world/update-world-state
             #(-> %
                  (assoc-in [:entities (:id player)] player)
                  (assoc-in [:entities (:id door)] door)))))
      (dialog/message game "No open door nearby"))))

(defmethod input/receive :player [player game input]
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
    ; \C (close-next-door game)
    :backspace (-> game world/pop-world-state center-viewport-player)
    :enter (do
             (swap! world/state (constantly (:world game)))
             game)
    (do (println "unmapped key" input)
        game)))
