(ns statehack.entity
  (:require [statehack.util :as util]))

(defn uuid [] (java.util.UUID/randomUUID))

;; (def entity-hierarchy (make-hierarchy))

;; (defn derive-entity [tag parent]
;;   (alter-var-root #'entity-hierarchy derive tag parent))

;; (defn entity-isa? [e parent]
;;   (isa? entity-hierarchy (:type e) parent))

;; (defn entity [type]
;;   {:id (uuid)
;;    :type type})

(defn entity []
  {:id (uuid)})

(defn position [e x y]
  {:pre [(>= x 0) (>= y 0)]}
  (assoc e :position [x y]))

(defn offset [x0 y0]
  (fn [entityf x1 y1]
    (entityf (+ x0 x1) (+ y0 y1))))

;; (defn collide-dispatch [game actor reactor]
;;   [(:type actor) (:type reactor)])

;; (defmulti collide #'collide-dispatch :hierarchy #'entity-hierarchy)
;; (defmethod collide :default [& args] args)

;; (defn blit-dispatch [x y]
;;   [(:type x) (:type y)])

;; (defmulti blit #'blit-dispatch :hierarchy #'entity-hierarchy)
;; (defmethod blit :default [x y] x)
