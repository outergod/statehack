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

(ns statehack.system.render
  "Rendering facility"
  (:require [halo.screen :as screen]
            [halo.graphics :as graphics]
            [statehack.system.world :as world]
            [statehack.system.messages :as messages]
            [statehack.system.status :as status]
            [statehack.system.input.receivers :as receivers]
            [statehack.system.unique :as unique]
            [statehack.system.sight :as sight]
            [statehack.system.levels :as levels]
            [statehack.system.memory :as memory]
            [statehack.util :as util]
            [statehack.entity :as entity]
            [statehack.algebra :as algebra]
            [clojure.walk :as walk]
            [clojure.set :as set]))

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

(defn frame [type & opts]
  (with-meta (apply hash-map opts) {:type type}))

(def layout
  "Screen layout"
  [(frame :layer :sections [(frame :status :size 1)
                            (frame :world :id :world :size :rest)
                            (frame :messages :size 5)])
   (frame :menu :size :rest)])

;;; Method `render`

(def render-hierarchy "Hierarchy for `render`" (make-hierarchy))

(defn derive-render
  "Derive for `render-hierarchy`"
  [tag parent]
  (alter-var-root #'render-hierarchy derive tag parent))

(defn render-dispatch
  "Dispatch for `render`"
  [game e]
  (e :renderable))

(defmulti render
  "Determine tile and color for rendering entity `e`"
  {:arglists '([game e])}
  #'render-dispatch :hierarchy #'render-hierarchy)

(defmethod render :default [_ x] (:renderable x))

;;; Method `blit`

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

;;; Canvas Transformations

(defn canvas-transform
  "Transform two-dimensional `canvas` from tile/color mapping to
  character/color vectors."
  [canvas]
  (mapv #(mapv (fn [{:keys [tile color char]}]
                 [(if tile (tiles tile) (str char)) color]) %)
        canvas))

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

(defn entity-blit
  "Blit entity `e` onto `canvas`."
  [game canvas e]
  (let [{:keys [position]} e]
    (canvas-update canvas position (constantly (render game e)))))

(defn splice
  "Splice `source` into `target` at `offset` using blit operation `f`."
  [offset f target source]
  {:pre [(>= offset 0)]}
  (let [[pre target] (split-at offset target)
        [target post] (split-at (count source) target)]
    (vec (concat pre (map f target source) post))))

(defn canvas-blit
  "Blit `source` canvas onto `target` at coordinates `[x0 y0]`."
  [target source [x0 y0]]
  {:pre [(>= x0 0) (>= y0 0)]}
  (splice y0 (partial splice x0 #(or %2 %1))
          target source))

(defn rect
  "Generate monotonous rectancle of proportions `[w h]` with tiles of
  `kind` and color `c`."
  [kind c [w h]]
  (vec (repeat h (vec (repeat w {:tile kind :color c})))))

(defn space
  "Create empty space rectangle of proportions `[w h]` with color `c`."
  [c [w h]]
  (rect :empty c [w h]))

(defn window
  "Create a visual window of proportions `[w h]` with border color `c`."
  [c [w h]]
  {:pre [(pos? w) (pos? h)]}
  (mapv #(mapv (fn [tile] {:tile tile :color c}) %)
        (apply concat
               [[(flatten [:tlcorner (repeat (- w 2) :hwall) :trcorner])]
                (repeat (- h 2) (flatten [:vwall (repeat (- w 2) :nihil) :vwall]))
                [(flatten [:blcorner (repeat (- w 2) :hwall) :brcorner])]])))

(defn entity-canvas
  "Transform collection of `entities` into a canvas based on their positions."
  [entities]
  (map (partial reduce blit) (vals (group-by :position entities))))

(defn reduce-entities
  "Reduce entities onto `canvas`"
  [game canvas es]
  (reduce (partial entity-blit game) canvas
          (entity-canvas es)))

(defn put-canvas
  "Write `canvas` at coordinates `[x0 y0]`."
  ([graphics canvas x0 y0]
     (doseq [[y row] (util/enumerate canvas)
             [x [s c]] (util/enumerate row)]
       (graphics/put graphics s (+ x x0) (+ y y0) :color c)))
  ([graphics canvas]
     (put-canvas graphics canvas 0 0)))

(defn center-offset
  "Calculate the offset needed to center `[w1 h1]` into a region of
  width `w2` and height `h2`."
  [[w1 h1] [w2 h2]]
  [(int (max (/ (- w2 w1) 2) 0))
   (int (max (/ (- h2 h1) 2) 0))])

(defn cut
  "Cut out part of `canvas`"
  [canvas [x y] [w h]]
  (let [[cw ch] (canvas-dimensions canvas)]
    (subvec (mapv #(subvec % (max 0 x) (min cw (+ x w)))
                  canvas)
            (max 0 y) (min ch (+ y h)))))

(defn fit-in
  "Cut and/or center `canvas`

  Into an area of width `w` and height `h` after centering it 
  on `[x y]`."
  [canvas [w h] [x y]]
  (let [base (rect :nihil 0 [w h])
        [cw ch] (canvas-dimensions canvas)
        [x0 y0] [(max 0 (min (- w cw) (- (int (/ w 2)) x)))
                 (max 0 (min (- h ch) (- (int (/ h 2)) y)))]]
    (canvas-blit base (cut canvas [(- cw w) (- ch h)] [w h]) [x0 y0])))

#_(defn visible?
  "Is `[x y]` within the visible area of the world section?"
  [game [x y]]
  (let [{:keys [graphics viewport]} game
        [x1 y1] viewport
        [x2 y2] (util/matrix-add viewport (size graphics :world))]
    (and (<= x1 x (dec x2)) (<= y1 y (dec y2)))))

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

(defn visible-world
  "Render the visible world of `e`"
  [game e]
  (let [canvas (space 7 (:foundation (levels/entity-floor game e)))
        mask (sight/visible-mask game e)
        es (entity/filter-capable [:position :renderable] (levels/floor-entities game (:floor e)))]
    (mask-canvas (reduce-entities game canvas es) mask)))

(defn dye
  "Change the color of all tiles in `canvas` to `c`"
  [canvas c]
  (walk/postwalk #(if (map? %) (assoc % :color c) %) canvas))

(defn memorized-world
  "Render the memorized world of `e`"
  [game e]
  (let [{:keys [floor foundation]} (levels/entity-floor game e)
        {:keys [entities coordinates]} (memory/entity-floor-memory e floor)
        canvas (reduce #(canvas-update %1 %2 (constantly {:tile :empty}))
                       (rect :nihil 0 foundation) coordinates)
        es (vals entities)]
    (dye (reduce-entities game canvas es) 8)))

(defn tilify-string
  "Make per-character tiles from string `s` using color `c`."
  [s c]
  [(mapv (fn [chr] {:char chr :color c}) s)])

#_(defn draw-cursor
  "Draw the cursor-capable entity in `es`."
  [game e es]
  (let [cursor (unique/unique-entity game :cursor)
        {:keys [screen graphics viewport]} game
        [w h] (:foundation (levels/entity-floor game e))
        cursor-position (:position cursor)
        section (receiver-section game)
        [x y] (util/matrix-add (position graphics section) cursor-position)]
    (case section
      :world (let [[x y] (util/matrix-add (util/matrix-subtract [x y] viewport)
                                          (center-offset [w h] (size graphics :world)))]
               (if (visible? game cursor-position)
                 (screen/move-cursor screen x y)
                 (screen/hide-cursor screen)))
      :menu (apply screen/move-cursor screen cursor-position)
      (screen/move-cursor screen x y))))

(defn draw-cursor
  "Draw the cursor"
  [game]
  (let [{:keys [screen graphics viewport]} game
        {[x y] :position} (unique/unique-entity game :cursor)]
    (if (entity/capable? (receivers/current game) :position)
      nil
      (screen/move-cursor screen x y))))

;;; Interface Methods

(def draw-hierarchy "Hierarchy for `draw` and `size`" (make-hierarchy))

(defn draw-dispatch
  "Dispatch for `draw`"
  [game frame [x y] [w h] canvas]
  (type frame))

(defmulti draw
  {:arglists '([game frame [x y] [w h] canvas])}
  #'draw-dispatch :hierarchy #'draw-hierarchy)

(defmethod draw :default [game frame [x y] [w h] canvas]
  (binding [*out* *err*] (println "Unknown frame type" (type frame)))
  canvas)

(defn size-dispatch
  "Dispatch for `size`"
  [frame h]
  (type frame))

(defmulti size
  {:arglists '([frame h])}
  #'size-dispatch :hierarchy #'draw-hierarchy)

(defmethod size :layer [{:keys [sections] :as layer} h]
  (let [sizes (map #(:size (size % h)) sections)
        fixed (reduce + (filter number? sizes))]
    (assoc layer
      :sections (map #(assoc %1 :size %2)
                     sections
                     (map #(if (= % :rest) (- h fixed) %) sizes)))))

(defmethod size :default [frame h] frame)

(defn offsets [sizes]
  (drop-last (reductions + 0 sizes)))

(defmethod draw clojure.lang.Sequential [game layers [x y] [w h] canvas]
  (reduce (fn [canvas layer] (draw game layer [x y] [w h] canvas)) canvas layers))

(defmethod draw :layer [game {:keys [sections] :as layer} [x y] [w h] canvas]
  (let [sizes (map :size sections)]
    (reduce (fn [canvas [frame y h]] (draw game frame [x y] [w h] canvas))
            canvas (partition 3 (interleave sections (offsets sizes) sizes)))))

(defmethod draw :status [game frame [x y] [w h] canvas]
  (canvas-blit canvas (tilify-string (status/text game (receivers/current game)) 7) (util/matrix-add [x y] [1 0])))

(defmethod draw :world [game frame [x y] [w h] canvas]
  (let [{:keys [viewport]} game
        e (unique/unique-entity game :player) ; TODO replace with seeing receiver
        world (canvas-blit (memorized-world game e) (visible-world game e) [0 0])
        view (fit-in world [w h] viewport)]
    (canvas-blit canvas view [x y])))

(defmethod draw :messages [game frame [x y] [w h] canvas]
  (if-let [d (unique/unique-entity game :dialog)]
    (-> canvas
        (canvas-blit (window 7 [w h]) [x y])
        (canvas-update (util/matrix-add [x y] [1 1]) (constantly {:tile :dialog-indicator :color 7}))
        (canvas-blit (tilify-string (messages/current d) 7) (util/matrix-add [x y] [2 1])))
    (reduce (fn [canvas [n m]]
            (canvas-blit canvas (tilify-string m 7) (util/matrix-add [x y] [1 n])))
            canvas (util/enumerate (messages/recent (unique/unique-entity game :log) 5)))))

(defmethod draw :menu [game frame [x y] [w h] canvas]
  (if-let [m (unique/unique-entity game :menu)]
    (canvas-blit canvas (window 7 [w (dec h)]) [0 1])
    canvas))

(defn system-layout
  "Determine the current layout"
  [{:keys [graphics] :as game}]
  (let [[_ h] (graphics/size graphics)
        layers (map #(size % h) layout)]
    (assoc game :layout {:layers (vec layers) :by-id (group-by :id (flatten layers))})))

(defn system-draw
  "Draw the whole UI."
  [{:keys [screen graphics layout] :as game}]
  (let [{:keys [entities] :as state} (world/state game)
        es (vals entities)
        e (unique/unique-entity game :player)
        [w h] (graphics/size graphics)
        canvas (rect :nihil 0 [w h])]
    (try
      (->> canvas (draw game (layout :layers) [0 0] [w h]) canvas-transform (put-canvas graphics))
      #_(draw-cursor game e es)
      (screen/refresh screen)
      (catch Exception e
        (throw (ex-info "Exception in rendering" {:state state} e)))))
  game)

#_(defn center-on
  "Determine the offset coordinates requires to center the world area
  on `[x y]`."
  [graphics [x y]]
  (let [[w h] (size graphics :world)]
    [(- x (int (/ w 2))) (- y (int (/ h 2)))]))

#_(defn into-bounds
  "Snap back `[x y]` into the visible screen area of `section` given
  a foundation of size `[w h]`, if necessary."
  [graphics section [w h] [x y]]
  (let [[sw sh] (size graphics section)
        x (cond (or (< x 0)
                    (<= w sw)) 0
                (>= (+ x sw) w) (- w sw)
                :default x)
        y (cond (or (< y 0)
                    (<= h sh)) 0
                (>= (+ y sh) h) (- h sh)
                :default y)]
    [x y]))

(defmethod render :humanoid [& _] {:tile :humanoid :color 7})
(defmethod render :serv-bot [& _] {:tile :serv-bot :color 160})
(defmethod render :corpse [& _] {:tile :corpse :color 88})

(doseq [d [:hdoor :vdoor]]
  (derive-render d :door))

(doseq [w [:tlcorner :trcorner :blcorner :brcorner :hwall :vwall
           :hdcross :hucross :vrcross :vlcross :cross :swall]]
  (derive-render w :wall))

(defmethod render :wall [game wall]
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

(defmethod render :door [game {:keys [open] :as door}]
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

(defmethod render :weapon [game e]
  {:tile :weapon
   :color 15})
