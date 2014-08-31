(ns statehack.entity
  (:require [statehack.util :as util]))

(defmulti tick :type)
(defmethod tick :default [_] nil)

(def entity-hierarchy (make-hierarchy))

(defn derive-entity [tag parent]
  (alter-var-root #'entity-hierarchy derive tag parent))

(defn entity [type x y]
  {:pre [(>= x 0) (>= y 0)]}
  {:type type
   :position [x y]})

(defn offset [x0 y0]
  (fn [entityf x1 y1]
    (entityf (+ x0 x1) (+ y0 y1))))

(defmulti render :type :hierarchy #'entity-hierarchy)

(defn collide-dispatch [game actor reactor]
  [(:type actor) (:type reactor)])

(defmulti collide #'collide-dispatch :hierarchy #'entity-hierarchy)
(defmethod collide :default [& args] args)

(defn player [x y]
  (entity :player x y))

(defmethod render :player [_] :player)
