(ns statehack.system.movement
  (:require [statehack.entity :as entity]
            [statehack.system.render :as render]
            [statehack.system.viewport :as viewport]
            [statehack.system.dialog :as dialog]
            [statehack.system.world :as world]
            [statehack.util :as util]
            [clojure.set :as set]))

(def move-hierarchy (make-hierarchy))

(defn- available-moves-dispatch [game e]
  (-> e :mobile :type))

(defmulti available-moves #'available-moves-dispatch :hierarchy #'move-hierarchy)
(defmethod available-moves :default [& _] nil)

(defn move [game e [x y]]
  (world/update-entity-component game e :position util/matrix-add [x y]))

(defn obstacles [es]
  (->> es (entity/filter-capable [:obstacle]) (remove :open)))

(defn inbound-moves [game e]
  (let [f (:foundation (world/current-world-state game))]
    (set (filter #(render/in-bounds? f (util/matrix-add (:position e) %))
                 world/neighbors))))

(defmethod available-moves :humanoid [game e]
  (let [es (obstacles (world/entity-neighbors game e))
        ds (set (map #(world/entity-delta % e) es))]
    (into {} (map (fn [pos] [pos #(move % e pos)])
                  (set/difference (inbound-moves game e) ds)))))

(defn move-next [game sel]
  (let [{:keys [targets]} (:mobile sel)
        es (:entities (world/current-world-state game))
        targets (concat (rest targets) [(first targets)])
        e (es (first targets))]
    (-> game
        (world/update-entity-component sel :mobile assoc :targets targets)
        (world/update-entity-component sel :position (constantly (:position e))))))

(defn unavailable-moves [game e]
  (let [os (obstacles (world/entity-neighbors game e))
        cs (set/difference world/neighbors (inbound-moves game e))]
    (merge (into {} (map (fn [o] [(world/entity-delta o e) #(dialog/message % "There's an obstacle in the way")]) os))
           (into {} (map (fn [c] [c #(dialog/message % "Somehow, you can't move here...")]) cs)))))

(defn update-cursor [game]
  (let [{:keys [entities receivers]} (world/current-world-state game)
        r (entities (first receivers))
        e (first (filter #(= (get-in % [:mobile :type]) :cursor) (vals entities)))
        [x y] (if (entity/capable? r :messages)
                (render/message-cursor-position game r)
                (render/entity-cursor-position r))]
    (world/update-entity-component game e :position (constantly [x y]))))
