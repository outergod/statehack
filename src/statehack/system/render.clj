(ns statehack.system.render
  (:require [halo.screen :as screen]
            [halo.graphics :as graphics]
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
   :dialog-indicator "⌐"
   :spell-up "⁀"
   :spell-down "‿"
   :spell-left "("
   :spell-right ")"})

(def render-hierarchy (make-hierarchy))

(defn derive-render [tag parent]
  (alter-var-root #'render-hierarchy derive tag parent))

(defn render-dispatch [game e]
  (e :renderable))

(defmulti render #'render-dispatch :hierarchy #'render-hierarchy)
(defmethod render :humanoid [& _] {:tile :humanoid :color 7})

(defn- blit-dispatch [e1 e2]
  [(get-in e1 [:renderable :tile]) (get-in e2 [:renderable :tile])])

(defmulti blit #'blit-dispatch :hierarchy #'render-hierarchy)
(defmethod blit :default [x _] x)

(defn draw [canvas]
  (map #(map (fn [{:keys [tile color]}] [(tiles tile) color]) %) canvas))

(defn entity-blit [game canvas e]
  (let [{:keys [position]} e]
    (update-in canvas (reverse position) (constantly (render game e)))))

(defn canvas-blit [canvas-1 canvas-2 x0 y0]
  {:pre [(>= x0 0) (>= y0 0)]}
  (let [[pre canvas-2] (split-at y0 canvas-2)
        [canvas-2 post] (split-at (count canvas-1) canvas-2)]
    (concat
     pre
     (map (fn [row-1 row-2]
            (let [[pre row-2] (split-at x0 row-2)]
              (concat pre (map #(or %1 %2) row-1 row-2))))
          canvas-1 canvas-2)
     post)))

(defn rect [kind c w h]
  (vec (repeat h (vec (repeat w {:tile kind :color c})))))

(defn space [c w h]
  (rect :empty c w h))

(defn window [c w h]
  {:pre [(pos? w) (pos? h)]}
  (map #(map (fn [tile] {:tile tile :color 7}) %)
       (apply concat
              [[(flatten [:tlcorner (repeat (- w 2) :hwall) :trcorner])]
               (repeat (- h 2) (flatten [:vwall (repeat (- w 2) :nihil) :vwall]))
               [(flatten [:blcorner (repeat (- w 2) :hwall) :brcorner])]])))

(defn move [x coll]
  (if (neg? x)
    (concat (repeat (Math/abs x) nil) coll)
    (drop x coll)))

(defn entity-canvas [entities]
  (map (partial reduce blit) (vals (group-by :position entities))))

(defmacro drawing [screen & body]
  `(do
     ~@body
     (screen/refresh ~screen)))

(def enumeration (iterate inc 0))

(defn put-canvas
  ([graphics canvas x0 y0]
     (doseq [[row y] (map list canvas enumeration)
             [[s c] x] (map list row enumeration)]
       (graphics/put graphics s (+ x x0) (+ y y0) :color c)))
  ([graphics canvas]
     (put-canvas graphics canvas 0 0)))

(defn- draw-objects [game es]
  (let [{:keys [graphics viewport]} game
        {:keys [foundation]} (world/current-world-state game)
        es (entity/filter-capable [:position :renderable] es)
        world (reduce (partial entity-blit game) foundation
                      (entity-canvas es))
        [x y] viewport
        view (map (partial move x) (move y world))]
    (put-canvas graphics (draw view))))

(defn- draw-interface [game es]
  (let [{:keys [graphics]} game
        [w h] (graphics/size graphics)]
    (doseq [e (entity/filter-capable [:renderable] es)]
      (case (:renderable e)
        :dialog (let [window (window 7 w 5)]
                  (put-canvas graphics (draw window) 0 (- h 5))
                  (graphics/put graphics (tiles :dialog-indicator) 1 (- h 4))
                  (graphics/put graphics (dialog/current e) 2 (- h 4)))
        :status (do
                  (put-canvas graphics (draw (rect :nihil 0 w 1)))
                  (graphics/put graphics (status/text game e) 1 0))
        true))))

(defn message-cursor-position [game e]
  (let [[_ h] (screen/size (:screen game))]
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

(defn center [graphics [x y]]
  (let [[w h] (graphics/size graphics)]
    [(- x (/ w 2)) (- y (/ h 2))]))

(defn into-bounds [canvas scr [x y]]
  (let [[sw sh] (screen/size scr)
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
  {:tile (condp set/subset? (set (map #(world/entity-delta % wall) (entity/filter-capable [:room] (world/entity-neighbors game wall))))
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
           :swall)
   :color 15})

(defmethod render :door [game {:keys [open] :as door}]
  {:tile (if open :open-door
             (condp set/subset? (set (map #(world/entity-delta % door) (entity/filter-capable [:room] (world/entity-neighbors game door))))
               #{[1 0] [-1 0]} :hdoor
               #{[0 1] [0 -1]} :vdoor
               :door))
   :color 15})

(defmethod blit [:humanoid :door] [& es]
  (first (filter #(= (get-in % [:renderable :type]) :humanoid) es)))

(defmethod blit [:door :humanoid] [& es]
  (first (filter #(= (get-in % [:renderable :type]) :humanoid) es)))
