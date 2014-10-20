(ns statehack.system.obstacle
  "Obstacle facility")

(def obstacle?-hierarchy "Hierarchy for `obstacle?`" (make-hierarchy))

(defn derive-obstacle?
  "Derive for `obstacle?-hierarchy`"
  [tag parent]
  (alter-var-root #'obstacle?-hierarchy derive tag parent))

(def obstacle?-dispatch
  "Dispatch for `obstacle?`"
  :obstacle)

(defmulti obstacle?
  "Is entity an obstacle?"
  {:arglists '([e])}
  #'obstacle?-dispatch :hierarchy #'obstacle?-hierarchy)

(defmethod obstacle? :default [_] false)
(defmethod obstacle? true [_] true)
(defmethod obstacle? false [_] false)

(defmethod obstacle? :door [e] (not (:open e)))

(defn filter-obstacles [es]
  (filter obstacle? es))
