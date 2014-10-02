(ns statehack.system.render
  (:require [lanterna.screen :as screen]
            [statehack.system.world :as world]
            [statehack.util :as util]
            [statehack.entity :as entity]
            [statehack.system.dialog :as dialog]
            [statehack.system.status :as status]
            [clojure.set :as set]))

(def tiles
  {:humanoid "@"
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
   :door "+"
   :open-door "▒"
   :swall "▢"
   :dialog-indicator "⌐"})

(def render-hierarchy (make-hierarchy))

(defn derive-render [tag parent]
  (alter-var-root #'render-hierarchy derive tag parent))

(defn render-dispatch [game e]
  (e :renderable))

(defmulti render #'render-dispatch :hierarchy #'render-hierarchy)
(defmethod render :humanoid [& _] :humanoid)

(defn- blit-dispatch [e1 e2]
  [(e1 :renderable) (e2 :renderable)])

(defmulti blit #'blit-dispatch :hierarchy #'render-hierarchy)
(defmethod blit :default [x _] x)

(defn draw [canvas]
  (map #(map tiles %) canvas))

(defn canvas-blit [game canvas e]
  (let [{:keys [position]} e]
    (update-in canvas (reverse position) (constantly (render game e)))))

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

(defmacro drawing [scr & body]
  `(do
     (screen/clear ~scr)
     ~@body
     (screen/redraw ~scr)))

(defn- draw-objects [game es]
  (let [{:keys [screen viewport]} game
        {:keys [foundation]} (world/current-world-state game)
        player (world/player-entity game)
        es (entity/filter-capable [:position :renderable] es)
        world (reduce (partial canvas-blit game) foundation
                      (entity-canvas es))
        [x y] viewport
        view (map (partial move x) (move y world))]
    (screen/put-sheet screen 0 0 (draw view))))

(defn- draw-interface [game es]
  (let [{:keys [screen]} game
        [w h] (screen/get-size screen)]
    (doseq [e (entity/filter-capable [:renderable] es)]
      (case (:renderable e)
        :dialog (let [window (window w 5)]
                  (screen/put-sheet screen 0 (- h 5) (draw window))
                  (screen/put-string screen 1 (- h 4) (tiles :dialog-indicator))
                  (screen/put-string screen 2 (- h 4) (dialog/current e)))
        :status (do
                  (screen/put-sheet screen 0 0 (draw (rect :nihil w 1)))
                  (screen/put-string screen 1 0 (status/text game e)))
        true))))

(defn message-cursor-position [game e]
  (let [[_ h] (screen/get-size (:screen game))]
    [(+ (count (first (:messages e))) 2) (- h 4)]))

(defn draw-cursor [game es]
  (let [cursor (first (filter #(= (-> % :mobile :type) :cursor) es))
        {:keys [screen viewport]} game
        {:keys [position]} cursor]
    (apply screen/move-cursor screen (util/matrix-subtract position viewport))))

(defn system [{:keys [screen] :as game}]
  (let [{:keys [entities] :as state} (world/current-world-state game)
        es (vals entities)]
    (try (drawing screen
           (draw-objects game es)
           (draw-interface game es)
           (draw-cursor game es))
         (catch Exception e
           (throw (ex-info "Exception in rendering" {:state state} e)))))
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

(defn in-bounds? [canvas [x y]]
  (and (>= x 0) (>= y 0)
       (< x (count (first canvas))) (< y (count canvas))))

(doseq [d [:hdoor :vdoor]]
  (derive-render d :door))

(doseq [w [:tlcorner :trcorner :blcorner :brcorner :hwall :vwall
           :hdcross :hucross :vrcross :vlcross :cross :swall]]
  (derive-render w :wall))

(defmethod render :wall [game wall]
  (condp set/subset? (set (map #(world/entity-delta % wall) (filter :room (world/entity-neighbors game wall))))
    #{[1 0] [-1 0] [0 1] [0 -1]} :cross
    #{[1 0] [-1 0] [0 1]} :hdcross
    #{[1 0] [-1 0] [0 -1]} :hucross
    #{[0 -1] [0 1] [1 0]} :vrcross
    #{[0 -1] [0 1] [-1 0]} :vlcross
    #{[1 0] [0 1]} :tlcorner
    #{[-1 0] [0 1]} :trcorner
    #{[0 -1] [1 0]} :blcorner
    #{[-1 0] [0 -1]} :brcorner
    #{[1 0]} :hwall #{[-1 0]} :hwall
    #{[0 -1]} :vwall #{[0 1]} :vwall
    :swall))

(defmethod render :door [game {:keys [open] :as door}]
  (if open :open-door
      (condp set/subset? (set (map #(world/entity-delta % door) (filter :room (world/entity-neighbors game door))))
        #{[1 0] [-1 0]} :hdoor
        #{[0 1] [0 -1]} :vdoor
        :door)))

(defmethod blit [:humanoid :door] [& es]
  (first (filter #(= (:renderable %) :humanoid) es)))

(defmethod blit [:door :humanoid] [& es]
  (first (filter #(= (:renderable %) :humanoid) es)))
