(ns statehack.system.render
  (:require [halo.screen :as screen]
            [halo.graphics :as graphics]
            [statehack.system.world :as world]
            [statehack.util :as util]
            [statehack.entity :as entity]
            [statehack.system.messages :as messages]
            [statehack.system.status :as status]
            [statehack.system.input.receivers :as receivers]
            [statehack.system.unique :as unique]
            [clojure.set :as set]))

(def tiles
  {:humanoid "@"
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
   :open-door "▒"
   :swall "▢"
   :dialog-indicator "⌐"
   :spell-up "⁀"
   :spell-down "‿"
   :spell-left "("
   :spell-right ")"})

(def layout
  {:sizes
   {:status 1
    :world :rest
    :messages 5}
   :order [:status :world :messages]})

(defn size [graphics section]
  (let [[w h] (graphics/size graphics)
        {:keys [sizes]} layout
        fixed (reduce + (filter integer? (vals sizes)))
        s (sizes section)]
    [w
     (if (= s :rest)
       (- h fixed)
       s)]))

(defn position [graphics section]
  (let [{:keys [sizes order]} layout
        pre (take-while (partial not= section) order)
        height (comp second (partial size graphics))]
    [0 (reduce + (map height pre))]))

(defn proportions [graphics]
  (let [sections (:order layout)]
    (into {} (map (fn [s] [s {:size (size graphics s)
                              :position (position graphics s)}])
                  sections))))

(def render-hierarchy (make-hierarchy))

(defn derive-render [tag parent]
  (alter-var-root #'render-hierarchy derive tag parent))

(defn render-dispatch [game e]
  (e :renderable))

(defmulti render #'render-dispatch :hierarchy #'render-hierarchy)

(def ^{:doc "Precedence of blit operations"} blit-order {})

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

(defn draw [canvas]
  (mapv #(mapv (fn [{:keys [tile color char]}]
                 [(if tile (tiles tile) (str char)) color]) %)
        canvas))

(defn canvas-update [canvas [x y] f]
  (update-in canvas [y x] f))

(defn entity-blit [game canvas e]
  (let [{:keys [position]} e]
    (canvas-update canvas position (constantly (render game e)))))

(defn splice [offset f target source]
  {:pre [(>= offset 0)]}
  (let [[pre target] (split-at offset target)
        [target post] (split-at (count source) target)]
    (vec (concat pre (map f target source) post))))

(defn canvas-blit [target source [x0 y0]]
  {:pre [(>= x0 0) (>= y0 0)]}
  (splice y0 (partial splice x0 #(or %2 %1))
          target source))

(defn rect [kind c [w h]]
  (vec (repeat h (vec (repeat w {:tile kind :color c})))))

(defn space [c [w h]]
  (rect :empty c [w h]))

(defn window [c [w h]]
  {:pre [(pos? w) (pos? h)]}
  (mapv #(mapv (fn [tile] {:tile tile :color c}) %)
        (apply concat
               [[(flatten [:tlcorner (repeat (- w 2) :hwall) :trcorner])]
                (repeat (- h 2) (flatten [:vwall (repeat (- w 2) :nihil) :vwall]))
                [(flatten [:blcorner (repeat (- w 2) :hwall) :brcorner])]])))

(defn entity-canvas [entities]
  (map (partial reduce blit) (vals (group-by :position entities))))

(defn put-canvas
  ([graphics canvas x0 y0]
     (doseq [[y row] (util/enumerate canvas)
             [x [s c]] (util/enumerate row)]
       (graphics/put graphics s (+ x x0) (+ y y0) :color c)))
  ([graphics canvas]
     (put-canvas graphics canvas 0 0)))

(defn center-offset
  "Calculate the offset needed to center `canvas` into a region of
  width `w` and height `h`."
  [canvas [w h]]
  (let [cw (count (first canvas))
        ch (count canvas)]
    [(int (max (/ (- w cw) 2) 0))
     (int (max (/ (- h ch) 2) 0))]))

(defn fit-in
  "Cut and/or center `canvas` into an area of width `w` and height `h`
  after moving it by offset `[x0 y0]`."
  [canvas [x0 y0] [w h]]
  (let [base (rect :nihil 0 [w h])
        cw (count (first canvas))
        ch (count canvas)]
    (canvas-blit base (subvec (mapv #(subvec % x0 (min (+ x0 w) cw))
                                    canvas)
                              y0 (min (+ y0 h) ch))
                 (center-offset canvas [w h]))))

(defn visible?
  "Is `[x y]` within the visible area of the world section?"
  [game [x y]]
  (let [{:keys [graphics viewport]} game
        [x1 y1] viewport
        [x2 y2] (util/matrix-add viewport (size graphics :world))]
    (and (<= x1 x (dec x2)) (<= y1 y (dec y2)))))

(defn- draw-world [game es canvas]
  (let [{:keys [graphics viewport]} game
        {:keys [foundation]} (world/state game)
        es (entity/filter-capable [:position :renderable] es)
        world (reduce (partial entity-blit game) foundation
                      (entity-canvas es))
        [x y] viewport
        [w h] (size graphics :world)
        view (fit-in world [x y] [w h])
        [x0 y0] (position graphics :world)]
    (canvas-blit canvas view [x0 y0])))

(defn tilify-string [s c]
  [(mapv (fn [chr] {:char chr :color c}) s)])

(defn- draw-status [game canvas e [x y]]
  (canvas-blit canvas (tilify-string (status/text game e) 7) (util/matrix-add [x y] [1 0])))

(defn- draw-log [canvas e [x y]]
  (reduce (fn [canvas [n m]]
            (canvas-blit canvas (tilify-string m 7) (util/matrix-add [x y] [1 n])))
          canvas (enumerate (messages/recent e 5) #_(take 5 (:messages e)))))

(defn- draw-dialog [canvas e [x y] [w h]]
  (-> canvas
      (canvas-blit (window 7 [w h]) [x y])
      (canvas-update (util/matrix-add [x y] [1 1]) (constantly {:tile :dialog-indicator :color 7}))
      (canvas-blit (tilify-string (messages/current e) 7) (util/matrix-add [x y] [2 1]))))

(defn- draw-interface [game es canvas]
  (let [{:keys [graphics]} game
        {:keys [status world messages]} (proportions graphics)
        dialog-visible? (unique/unique-entity game :dialog)]
    (reduce
     (fn [canvas e]
       (case (:renderable e)
         :status (draw-status game canvas e (:position status))
         :log (if dialog-visible?
                canvas
                (draw-log canvas e (:position messages)))
         :dialog (draw-dialog canvas e (:position messages) (:size messages))
         canvas))
       canvas (entity/filter-capable [:renderable] es))))

(defn receiver-section [game]
  (if (entity/capable? (receivers/current game) :messages)
    :messages :world))

(defn draw-cursor [game es]
  (let [cursor (first (filter #(= (-> % :mobile :type) :cursor) es))
        {:keys [screen graphics viewport]} game
        {:keys [foundation]} (world/state game)
        cursor-position (:position cursor)
        section (receiver-section game)
        [x y] (util/matrix-add (position graphics section) cursor-position)]
    (if (= section :world)
      (let [[x y] (util/matrix-add (util/matrix-subtract [x y] viewport)
                                   (center-offset foundation (size graphics :world)))]
        (if (visible? game cursor-position)
          (screen/move-cursor screen x y)
          (screen/hide-cursor screen)))
      (screen/move-cursor screen x y))))

(defn system [{:keys [screen graphics] :as game}]
  (let [{:keys [entities] :as state} (world/state game)
        es (vals entities)
        [w h] (graphics/size graphics)
        canvas (rect :nihil 0 [w h])]
    (try
      (->> canvas (draw-world game es) (draw-interface game es) draw (put-canvas graphics))
      (draw-cursor game es)
      (screen/refresh screen)
      (catch Exception e
        (throw (ex-info "Exception in rendering" {:state state} e)))))
  game)

(defn center-on [graphics [x y]]
  (let [[w h] (size graphics :world)]
    [(- x (int (/ w 2))) (- y (int (/ h 2)))]))

(defn into-bounds [graphics section canvas [x y]]
  (let [[sw sh] (size graphics section)
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

(defmethod render :humanoid [& _] {:tile :humanoid :color 7})
(defmethod render :corpse [& _] {:tile :corpse :color 88})

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

(blit-precedence :humanoid :door)
(blit-precedence :humanoid :corpse)
(blit-precedence :corpse :door)
