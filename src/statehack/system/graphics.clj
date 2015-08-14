;;;; This file is part of statehack.
;;;;
;;;; statehack is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; statehack is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with statehack.  If not, see <http://www.gnu.org/licenses/>.

(ns statehack.system.graphics
  "Graphics facility"
  (:require [halo.screen :as screen]
            [halo.graphics :as graphics]
            [statehack.system.world :as world]
            [statehack.system.layout :as layout]
            [statehack.system.levels :as levels]
            [statehack.system.memory :as memory]
            [statehack.system.sight :as sight]
            [statehack.system.unique :as unique]
            [statehack.system.status :as status]
            [statehack.system.messages :as messages]
            [statehack.util :as util]
            [statehack.entity :as entity]
            [statehack.algebra :as algebra]
            [clojure.walk :as walk]
            [clojure.set :as set]
            [taoensso.timbre :as timbre]))

(def render-hierarchy "Hierarchy for `render`" (make-hierarchy))

(defn render-dispatch
  "Dispatch for `render`"
  [game e]
  (e :type))

(defmulti render
  "Render layout element `e`"
  {:arglists '([game e])}
  #'render-dispatch :hierarchy #'render-hierarchy)

(def draw-hierarchy "Hierarchy for `draw`" (make-hierarchy))

(defn draw-dispatch
  "Dispatch for `draw`"
  [game binding dimensions]
  binding)

(defmulti draw
  "Draw `binding`"
  {:arglists '([game binding dimensions])}
  #'draw-dispatch :hierarchy #'draw-hierarchy)

(defmethod render :box [game {:keys [children]}]
  (mapcat (partial render game) children))

(defmethod render :stack [game {:keys [children]}]
  (mapcat (partial render game) children))

(defmethod render :view [game {:keys [binding dimensions]}]
  (draw game binding dimensions))

(def blit-order "Precedence of blit operations" {})

(defn blit-precedence
  "Define precedence of renderable `r1` over `r2`"
  [r1 r2]
  (alter-var-root #'blit-order assoc #{r1 r2} r1))

(defn blit
  "Evaluate to the entity with higher blit order"
  [e1 e2]
  (let [[r1 r2] (map :renderable [e1 e2])
        rs (blit-order #{r1 r2})]
    (condp = rs ; case does never match; why??
      r1 e1
      r2 e2
      (throw (ex-info "Blit order of entities undefined" {:entities [e1 e2]})))))

(def tiles
  "Mapping of tile keywords to characters"
  {:humanoid "@"
   :serv-bot "b"
   :corpse "%"
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
   :swall "▢"
   :dialog-indicator "⌐"
   :weapon ")"
   
   ; not in use
   :spell-up "⁀"
   :spell-down "‿"
   :spell-left "("
   :spell-right ")"
   :camera "⚷"
   :battery "🔋"})

(def tile-hierarchy "Hierarchy for `tile`" (make-hierarchy))

(defn derive-tile
  "Derive for `tile-hierarchy`"
  [tag parent]
  (alter-var-root #'tile-hierarchy derive tag parent))

(defn tile-dispatch
  "Dispatch for `tile`"
  [game e]
  (e :renderable))

(defmulti tile
  "Determine tile and color for rendering entity `e`"
  {:arglists '([game e])}
  #'tile-dispatch :hierarchy #'tile-hierarchy)

(defmethod tile :default [_ x] (:renderable x))

(defn transform
  "Transform two-dimensional `canvas` from tile/color mapping to
  character/color vectors."
  [canvas]
  (mapv #(mapv (fn [{:keys [tile color char]}]
                 [(if tile (tiles tile) (str char)) color]) %)
        canvas))

(defn dye
  "Change the color of all tiles in `canvas` to `c`"
  [canvas c]
  (walk/postwalk #(if (map? %) (assoc % :color c) %) canvas))

(defn canvas-dimensions
  "The dimensions of `canvas`"
  [canvas]
  [(count (first canvas)) (count canvas)])

(defn canvas-update
  "Update tile at coordinates [x y] in `canvas` with `f`

  Ignore coordinates outside bounds."
  [canvas [x y] f]
  (let [[w h] (canvas-dimensions canvas)]
    (if (and (< -1 x w) (< -1 y h))
      (update-in canvas [y x] f)
      canvas)))

(defn rect
  "Generate monotonous rectancle of proportions `[w h]` with tiles of
  `kind` and color `c`."
  [kind c [w h]]
  (vec (repeat h (vec (repeat w {:tile kind :color c})))))

(defn space
  "Create empty space rectangle of proportions `[w h]` with color `c`."
  [c [w h]]
  (rect :empty c [w h]))

(defn entity-canvas
  "Transform collection of `entities` into a canvas based on their positions."
  [entities]
  (map (partial reduce blit) (vals (group-by :position entities))))

(defn entity-blit
  "Blit entity `e` onto `canvas`."
  [game canvas e]
  (let [{:keys [position]} e]
    (canvas-update canvas position (constantly (tile game e)))))

(defn- reduce-entities
  "Reduce entities onto `canvas`"
  [game canvas es]
  (reduce (partial entity-blit game) canvas
          (entity-canvas es)))

(defn- splice
  "Splice `source` into `target` at `offset` using blit operation `f`."
  [offset f target source]
  {:pre [(>= offset 0)]}
  (let [[pre target] (split-at offset target)
        [target post] (split-at (count source) target)]
    (vec (concat pre (map f target source) post))))

(defn- canvas-blit
  "Blit `source` canvas onto `target` at coordinates `[x0 y0]`."
  ([target source [x0 y0]]
   {:pre [(>= x0 0) (>= y0 0)]}
   (splice y0 (partial splice x0 #(or %2 %1))
           target source))
  ([target source]
   (canvas-blit target source [0 0])))

(defn- center-offset
  "Calculate the offset needed to center `[w1 h1]` into a region of
  width `w2` and height `h2`."
  [[w1 h1] [w2 h2]]
  [(int (max (/ (- w2 w1) 2) 0))
   (int (max (/ (- h2 h1) 2) 0))])

(defn- fit-in
  "Cut and/or center `canvas` into an area of width `w` and height
  `h` after centering it on `[x y]`."
  [canvas [w h] [x y]]
  (let [base (rect :nihil 0 [w h])
        [cw ch] (canvas-dimensions canvas)
        [x0 y0] (map (partial max 0) (util/matrix-subtract [x y] [(/ w 2) (/ h 2)]))
        [x1 y1] [(min (+ x0 w) cw) (min (+ y0 h) ch)]]
    (canvas-blit base (subvec (mapv #(subvec % x0 x1) canvas) y0 y1)
                 (center-offset [cw ch] [w h]))))

(defn mask-canvas
  "Apply `mask` to `canvas`, making everything outside the mask invisible"
  [canvas mask]
  (mapv (fn [y row]
          (mapv (fn [x tile]
                  (if (mask [x y])
                    tile
                    nil))
                util/enumeration row))
        util/enumeration canvas))

(defn memorized-world
  "Render the memorized world of `e`"
  [game e]
  (let [{:keys [floor foundation]} (levels/entity-floor game e)
        {:keys [entities coordinates]} (memory/entity-floor-memory e floor)
        canvas (reduce #(canvas-update %1 %2 (constantly {:tile :empty}))
                       (rect :nihil 0 foundation) coordinates)
        es (vals entities)]
    (dye (reduce-entities game canvas es) 8)))

(defn visible-world
  "Render the visible world of `e`"
  [game e]
  (let [canvas (space 7 (:foundation (levels/entity-floor game e)))
        mask (sight/visible-mask game e)
        es (entity/filter-capable [:position :renderable] (levels/floor-entities game (:floor e)))]
    (mask-canvas (reduce-entities game canvas es) mask)))

(defn put-canvas
  "Write `canvas` at coordinates `[x0 y0]`."
  ([graphics canvas x0 y0]
     (doseq [[y row] (util/enumerate canvas)
             [x [s c]] (util/enumerate row)]
       (graphics/put graphics s (+ x x0) (+ y y0) :color c)))
  ([graphics canvas]
   (put-canvas graphics canvas 0 0)))

(defn tilify-string
  "Make per-character tiles from string `s` using color `c`."
  [s c]
  (mapv (fn [chr] {:char chr :color c}) s))

;; World

(defmethod draw :world [{:keys [graphics viewport] :as game} _ dimensions]
  (let [player (unique/unique-entity game :player)
        world (canvas-blit (memorized-world game player) (visible-world game player))]
    (fit-in world dimensions viewport)))

;; Messages

(defmethod draw :messages [game _ [w h]]
  (let [log (unique/unique-entity game :log)
        canvas (rect :nihil 0 [w h])]
    (canvas-blit canvas (map #(tilify-string % 7) (messages/recent log 5)))))

;; Status

(defmethod draw :status [game _ [w h]]
  (let [player (unique/unique-entity game :player)
        canvas (rect :nihil 0 [w h])]
    (canvas-blit canvas [(tilify-string (status/text game player) 7)])))

;; Tile

(defmethod tile :humanoid [& _] {:tile :humanoid :color 7})
(defmethod tile :serv-bot [& _] {:tile :serv-bot :color 160})
(defmethod tile :corpse [& _] {:tile :corpse :color 88})

(doseq [d [:hdoor :vdoor]]
  (derive-tile d :door))

(doseq [w [:tlcorner :trcorner :blcorner :brcorner :hwall :vwall
           :hdcross :hucross :vrcross :vlcross :cross :swall]]
  (derive-tile w :wall))

(defmethod tile :wall [game wall]
  {:tile (condp set/subset? (set (map #(world/entity-delta % wall)
                                      (entity/filter-capable [:room] (world/entity-neighbors game wall))))
           algebra/neighbor-deltas :nihil
           (set/difference algebra/neighbor-deltas #{[1 -1]}) :blcorner
           (set/difference algebra/neighbor-deltas #{[-1 -1]}) :brcorner
           (set/difference algebra/neighbor-deltas #{[1 1]}) :tlcorner
           (set/difference algebra/neighbor-deltas #{[-1 1]}) :trcorner
           
           (set/difference algebra/neighbor-deltas #{[-1 -1] [1 -1] [0 -1]}) :hwall
           (set/difference algebra/neighbor-deltas #{[1 1] [-1 1] [0 1]}) :hwall
           (set/difference algebra/neighbor-deltas #{[-1 0] [-1 -1] [-1 1]}) :vwall
           (set/difference algebra/neighbor-deltas #{[1 0] [1 1] [1 -1]}) :vwall

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

(defmethod tile :door [game {:keys [open] :as door}]
  {:tile (condp set/subset? (set (map #(world/entity-delta % door) (entity/filter-capable [:room] (world/entity-neighbors game door))))
           #{[1 0] [-1 0]} :hdoor
           #{[0 1] [0 -1]} :vdoor
           :door)
   :color (if open 8 15)})

(blit-precedence :humanoid :door)
(blit-precedence :humanoid :corpse)
(blit-precedence :corpse :door)

(blit-precedence :humanoid :weapon)
(blit-precedence :weapon :corpse)

(defmethod tile :weapon [game e]
  {:tile :weapon
   :color 15})

;; System

(defn system [{:keys [screen graphics layout] :as game}]
  (put-canvas graphics (transform (render game layout)))
  (screen/refresh screen)
  game)
