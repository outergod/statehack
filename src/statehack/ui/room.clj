(ns statehack.ui.room
  (:require [statehack.util :as util]
            [statehack.entity :as entity]
            [clojure.string :as str]))

(doseq [w '(tlcorner trcorner blcorner brcorner hwall vwall
            hdcross hucross vrcross vlcross cross swall)
        :let [type (keyword w)]]
  (eval
   `(do
      ~(entity/derive-entity type :wall)
      (defmethod entity/render ~type [_#] ~type)
      (def ~w (partial entity/entity ~type)))))

(entity/derive-entity :hdoor :door)
(entity/derive-entity :vdoor :door)

(defn hdoor [x y open?] (into {:open open?} (entity/entity :hdoor x y)))
(defn vdoor [x y open?] (into {:open open?} (entity/entity :vdoor x y)))

(defmethod entity/render :door [{:keys [type open] :as door}]
  (if open :open-door type))

(defn open-door-dispatch [game actor reactor]
  [(:type actor) (:type reactor)])

(defmulti open-door #'open-door-dispatch :hierarchy #'entity/entity-hierarchy)
(defmethod open-door [:player :door] [game player {:keys [open] :as door}]
  [game player (assoc door :open true)])

(defmethod entity/collide [:player :wall] [game player wall]
  (println "player ran into wall")
  [game player wall])

(defmethod entity/collide [:player :door] [game player {:keys [open position] :as door}]
  (if open
    [game (assoc player :position position) door]
    (open-door game player door)))

(defn room
  [x y w h]
  {:pre [(pos? w) (pos? h)]}
  (let [off (entity/offset x y)]
    (flatten [(off tlcorner 0 0) (off trcorner (inc w) 0)
              (off blcorner 0 (inc h)) (off brcorner (inc w) (inc h))
              (for [x (range 1 (inc w))]
                [(off hwall x 0) (off hwall x (inc h))])
              (for [y (range 1 (inc h))]
                [(off vwall 0 y) (off vwall (inc w) y)])])))

(def neighbors
  (set (remove #(= % [0 0])
               (apply concat
                      (for [y [-1 0 1]]
                        (for [x [-1 0 1]]
                          [x y]))))))

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
                     ((condp #(every? %2 %1) (set (filter #(-> % (util/matrix-add [x y]) find-wall) neighbors))
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
                      x1 y1)))))))))
