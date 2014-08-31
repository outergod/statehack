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
   :swall "▢"})

(defn draw [canvas]
  (map #(map tiles %) canvas))

(defn blit [canvas e]
  (let [{:keys [position]} e]
    (update-in canvas (reverse position) (constantly (entity/render e)))))

(defn rect [kind x y]
  (vec (repeat y (vec (repeat x kind)))))

(defn space [x y]
  (rect :empty x y))

(defn move [x coll]
  (if (neg? x)
    (concat (repeat (Math/abs x) nil) coll)
    (drop x coll)))

(defn entity-canvas [entities]
  (map (partial reduce entity/blit) (vals (group-by :position entities))))

(defn draw-game [scr game]
  (let [{:keys [world viewport]} game
        {:keys [foundation entities player]} (first world)
        world (reduce blit foundation
                      (entity-canvas (cons player (vals entities))))
        [x y] viewport
        view (map (partial move x) (move y world))]
    (screen/put-sheet scr 0 0 (draw view))
    (apply screen/move-cursor scr (util/matrix-subtract (:position player) viewport))))

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
