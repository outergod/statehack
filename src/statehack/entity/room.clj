(ns statehack.entity.room
  (:require [statehack.entity :refer :all]
            [statehack.component :as c]
            [clojure.string :as str]
            [statehack.util :as util]))

(defn wall [x y]
  (entity
   (c/renderable :wall)
   (c/position x y)
   (c/room)))

(defn door [x y open?]
  (entity
   (c/renderable :door)
   (c/position x y)
   (c/door open?)
   (c/room)))

(defn extract-room [s x0 y0]
   (let [lines (str/split-lines s)
         find-wall (memoize (fn [[x y]] (#{\X \O \o} (get-in lines [y x]))))]
     (filter identity
             (flatten
              (for [[row y] (partition 2 (interleave lines (range (count s))))
                    [token x] (partition 2 (interleave row (range (count row))))]
                (when-let [w (find-wall [x y])]
                  (let [[x1 y1] (util/matrix-add [x0 y0] [x y])]
                    (case w
                      \X (wall x1 y1)
                      \O (door x1 y1 true)
                      \o (door x1 y1 false)))))))))

(comment
 (doseq [w '(tlcorner trcorner blcorner brcorner hwall vwall
                      hdcross hucross vrcross vlcross cross swall)
         :let [type (keyword w)]]
   (eval
    `(do
       (defmethod render/render ~type [_#] ~type)
       (defn ~w [x# y#] (-> (entity/entity) (entity/position x# y#) (render/renderable ~type))))))

 (defmethod render/blit #{:player :wall} [& xs]
   (some #(and (= (:type %) :player) %) xs))

 (render/derive-render :hdoor :door)
 (render/derive-render :vdoor :door)

 (defn hdoor [x y open?] (into {:open open?} (-> (entity/entity :hdoor) (entity/position x y) (render/renderable :door))))
 (defn vdoor [x y open?] (into {:open open?} (-> (entity/entity :vdoor) (entity/position x y) (render/renderable :door))))

 (defmethod render/render :door [{:keys [type open] :as door}]
   (if open :open-door type))

 (defmethod render/blit [:player :door] [& xs]
   (some #(and (= (:type %) :player) %) xs))

 (defmethod render/blit [:door :player] [& xs]
   (some #(and (= (:type %) :player) %) xs))

 (defn toggle-door-dispatch [game actor reactor open]
   [(:type actor) (:type reactor)])

 (defmulti toggle-door #'toggle-door-dispatch :hierarchy #'entity/entity-hierarchy)
 (defmethod toggle-door [:player :door] [game player {:keys [open] :as door} open?]
   [game player (assoc door :open open?)])

 (defn close-candidates [game x y]
   (letfn [(isa-door? [e] (entity/entity-isa? e :door))]
     (seq (filter (every-pred isa-door? :open)
                  (world/direct-neighbors (world/current-world-state game) x y)))))

 (defmethod entity/collide [:player :wall] [game player wall]
   [game player wall])

 (defmethod entity/collide [:player :door] [game player {:keys [open position] :as door}]
   (if open
     [game (assoc player :position position) door]
     (toggle-door game player door true)))

 (defn extract-room [s x0 y0]
   (let [lines (str/split-lines s)
         find-wall (memoize (fn [[x y]] (#{\X \O \o} (get-in lines [y x]))))]
     (filter identity
             (flatten
              (for [[row y] (partition 2 (interleave lines (range (count s))))]
                (for [[token x] (partition 2 (interleave row (range (count row))))]
                  (when-let [w (find-wall [x y])]
                    (let [[x1 y1] (util/matrix-add [x0 y0] [x y])
                          [hwall vwall] (case w
                                          \X [hwall vwall]
                                          \O [#(hdoor %1 %2 true) #(vdoor %1 %2 true)]
                                          \o [#(hdoor %1 %2 false) #(vdoor %1 %2 false)])]
                      ((condp #(every? %2 %1) (set (filter #(-> % (util/matrix-add [x y]) find-wall) world/neighbors))
                         #{[1 0] [-1 0] [0 1] [0 -1]} cross
                         #{[1 0] [-1 0] [0 1]} hdcross
                         #{[1 0] [-1 0] [0 -1]} hucross
                         #{[0 -1] [0 1] [1 0]} vrcross
                         #{[0 -1] [0 1] [-1 0]} vlcross
                         #{[1 0] [0 1]} tlcorner
                         #{[-1 0] [0 1]} trcorner
                         #{[0 -1] [1 0]} blcorner
                         #{[-1 0] [0 -1]} brcorner
                         #{[1 0]} hwall #{[-1 0]} hwall
                         #{[0 -1]} vwall #{[0 1]} vwall
                         swall)
                       x1 y1))))))))))
