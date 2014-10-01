(ns statehack.system
  (:require [clojure.set :as set]
            [statehack.entity :as entity]))

(def behavior-systems {})
(def component-behaviors {})

(defn unregister-behavior [name]
  {:pre [(keyword? name)]}
  (alter-var-root #'behavior-systems dissoc name)
  (alter-var-root #'component-behaviors #(into {} (map (fn [[k v]] [k (disj v name)]) %))))

(defn register-behavior [name [& cs] reaction]
  {:pre [(keyword? name) (every? keyword? cs) (not (zero? (count cs)))
         (fn? reaction)]}
  (unregister-behavior name)
  (alter-var-root #'behavior-systems assoc name {:reaction reaction
                                                 :components cs})
  (doseq [c (set cs)]
    (alter-var-root #'component-behaviors #(merge-with set/union % {c (set [name])}))))

(defmacro behavior
  {:arglists '([name [& cs] [game entity component? [bindings*]] & body])}
  [name [& cs] [& args] & body]
  {:pre [(>= (count args) 2) (<= (count args) 4)]}
  (let [[game entity & args] args
        [c args] (if (symbol? (first args))
                   [(first args) (next args)]
                   [(gensym) args])
        bs (if-let [b (first args)]
             (if (vector? b)
               b
               (throw (IllegalArgumentException. "Optional fourth argument must be vector of bindings")))
             (gensym))]
    `(register-behavior (keyword '~name) [~@cs]
                        (fn [~game ~entity ~c ~bs] ~@body))))

(defn trigger [game c e]
  (reduce (fn [game cb]
            (println "Triggering behavior" cb "for component" c "in entity" (:id e))
            (let [{:keys [reaction components]} (behavior-systems cb)]
              (reaction game e c (map e components))))
          game (component-behaviors c)))
