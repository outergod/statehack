(ns statehack.util
  (:import [clojure.lang IPending]))

(defn matrix-add
  ([x] x)
  ([x y]
     (map + x y))
  ([x y & more]
     (reduce matrix-add (matrix-add x y) more)))

(defn matrix-subtract
  ([x] x)
  ([x y]
     (map - x y))
  ([x y & more]
     (reduce matrix-subtract (matrix-subtract x y) more)))

(defn index-by [f coll]
  (into {} (map (fn [o] [(f o) o]) coll)))

(defn separate
  [map & ks]
  (let [sep (select-keys map ks)]
    [(apply dissoc map ks) sep]))

; http://stackoverflow.com/a/25532104
(defn count-realized [s] 
  (loop [s s n 0] 
    (if (instance? IPending s)
      (if (and (realized? s) (seq s))
        (recur (rest s) (inc n))
        n)
      (if (seq s)
        (recur (rest s) (inc n))
        n))))
