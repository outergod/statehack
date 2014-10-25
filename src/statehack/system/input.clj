(ns statehack.system.input)

(def receive-hierarchy (make-hierarchy))

(defn derive-receive [tag parent]
  (alter-var-root #'receive-hierarchy derive tag parent))

(defn- receive-dispatch [game e input]
  (:input e))

(defmulti receive #'receive-dispatch :hierarchy #'receive-hierarchy)
