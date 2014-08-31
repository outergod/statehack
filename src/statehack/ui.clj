(ns statehack.ui
  (:require [lanterna.screen :as screen]
            [statehack.entity :as entity]
            [statehack.util :as util]))

(def tiles
  {:player "@"
   :nihil " "
   :empty "·"
   :hwall "─"
   :vwall "│"
   :tlcorner "╭"
   :trcorner "╮"
   :blcorner "╰"
   :brcorner "╯"
   :hdcross "┬"
   :hucross "┴"
   :vrcross "├"
   :vlcross "┤"
   :cross "┼"
   :vdoor "║"
   :hdoor "═"
   :open-door "▒"
   :swall "▢"
   :dialog-indicator "⌐"})

(defn draw [canvas]
  (map #(map tiles %) canvas))

(defn blit [canvas e]
  (let [{:keys [position]} e]
    (update-in canvas (reverse position) (constantly (entity/render e)))))

(defn rect [kind w h]
  (vec (repeat h (vec (repeat w kind)))))

(defn space [w h]
  (rect :empty w h))

(defn window [w h]
  {:pre [(pos? w) (pos? h)]}
  (apply concat
         [[(flatten [:tlcorner (repeat (- w 2) :hwall) :trcorner])]
          (repeat (- h 2) (flatten [:vwall (repeat (- w 2) :nihil) :vwall]))
          [(flatten [:blcorner (repeat (- w 2) :hwall) :brcorner])]]))

(defn move [x coll]
  (if (neg? x)
    (concat (repeat (Math/abs x) nil) coll)
    (drop x coll)))

(defn entity-canvas [entities]
  (map (partial reduce entity/blit) (vals (group-by :position entities))))

(defn- draw-game-dispatch [game]
  (-> game :world first :mode))

(defmulti draw-game #'draw-game-dispatch)

(defn- draw-world [game]
  (let [{:keys [screen world viewport]} game
        {:keys [foundation entities player]} (first world)
        world (reduce blit foundation
                      (entity-canvas (cons player (vals entities))))
        [x y] viewport
        view (map (partial move x) (move y world))]
    (screen/put-sheet screen 0 0 (draw view))
    (apply screen/move-cursor screen (util/matrix-subtract (:position player) viewport))))

(defmethod draw-game :world [game]
  (draw-world game))

(defmethod draw-game :dialog [game]
  (draw-world game)
  (let [{:keys [screen world]} game
        {:keys [messages]} (first world)
        [w h] (screen/get-size screen)
        window (window w 5)
        m (first messages)]
    (screen/put-sheet screen 0 (- h 5) (draw window))
    (screen/put-string screen 1 (- h 4) (tiles :dialog-indicator))
    (screen/put-string screen 2 (- h 4) m)
    (screen/move-cursor screen (+ (count m) 2) (- h 4))))

(defn center [scr [x y]]
  (let [[w h] (screen/get-size scr)]
    [(- x (/ w 2)) (- y (/ h 2))]))

(defn into-bounds [canvas scr [x y]]
  (let [[sw sh] (screen/get-size scr)
        fw (count (first canvas))
        fh (count canvas)
        x (cond (or (< x 0)
                    (<= fw sw)) 0
                (>= (+ x sw) fw) (- fw sw)
                :default x)
        y (cond (or (< y 0)
                    (<= fh sh)) 0
                (>= (+ y sh) fh) (- fh sh)
                :default y)]
    [x y]))

(defmacro drawing [scr & body]
  `(do
     (screen/clear ~scr)
     ~@body
     (screen/redraw ~scr)))
