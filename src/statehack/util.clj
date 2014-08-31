(ns statehack.util)

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
