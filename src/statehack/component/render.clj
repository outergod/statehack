(ns statehack.component.render
  (:require [lanterna.screen :as screen]
            [statehack.game.world :as world]
            [statehack.util :as util]
            [clojure.set :as set]))

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

(def render-hierarchy (make-hierarchy))

(defn derive-render [tag parent]
  (alter-var-root #'render-hierarchy derive tag parent))

(defn renderable [e type]
  (assoc e :renderable type))

(defmulti render :renderable :hierarchy #'render-hierarchy)
(defmethod render :player [_] :player)

(defn- blit-dispatch [x y]
  [(render x) (render y)])

(defmulti blit #'blit-dispatch :hierarchy #'render-hierarchy)
(defmethod blit :default [x y] x)

(defn draw [canvas]
  (map #(map tiles %) canvas))

(defn canvas-blit [canvas e]
  (let [{:keys [position]} e]
    (update-in canvas (reverse position) (constantly (render e)))))

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
  (map (partial reduce blit) (vals (group-by :position entities))))

(defn- render-system-dispatch [game]
  (:mode (world/current-world-state game)))

(defmulti render-system #'render-system-dispatch)

(defmacro drawing [scr & body]
  `(do
     (screen/clear ~scr)
     ~@body
     (screen/redraw ~scr)))

(defn- draw-world [game]
  (let [{:keys [screen viewport]} game
        {:keys [foundation entities player]} (world/current-world-state game)
        player (entities player)
        entities (filter #(set/subset? #{:renderable :position} (set (keys %)))
                         (vals entities))
        world (reduce canvas-blit foundation
                      (entity-canvas entities))
        [x y] viewport
        view (map (partial move x) (move y world))]
    (screen/put-sheet screen 0 0 (draw view))
    (apply screen/move-cursor screen (util/matrix-subtract (:position player) viewport))))

(defmethod render-system :world [{:keys [screen] :as game}]
  (drawing screen (draw-world game))
  game)

(defmethod render-system :dialog [{:keys [screen] :as game}]
  (drawing screen
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
  game)

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