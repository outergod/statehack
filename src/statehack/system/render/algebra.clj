(ns statehack.system.render.algebra
  "Algebraic algorithms for rasterized, linear planes"
  (:require [statehack.util :as util]))

(defn- filter-circle
  "Evaluate to all points in a square with length `(* r 2)` around
  `[x0 y0]` that satisfy distance predicate `pred` so that 
  `(pred distance-to-x0-y0 r)` is true."
  [[x0 y0] r pred]
  (let [start (* r -1) end (inc r)]
    (map (partial util/matrix-add [x0 y0])
         (filter (fn [[x y]]
                   (pred (Math/round (Math/sqrt (+ (Math/pow x 2) (Math/pow y 2)))) r))
                 (for [x (range start end) y (range start end)] [x y])))))

(defn circle
  "All points approximating to the circle around `[x0 y0]` with radius
  `r`."
  [[x0 y0] r]
  (filter-circle [x0 y0] r =))

(defn circle-area
  "All points within and including the circle around `[x0 y0]` with
  radius `r`."
  [[x0 y0] r]
  (filter-circle [x0 y0] r <=))

;; Much, much slower than `circle` for large `r`!
(defn bresenham-circle
  "All points approximating to the circle around `[x0 y0]` with radius
  `r`, using the midpoint circle algorithm."
  [[x0 y0] r]
  (loop [x r y 0 err (- 1 r) acc []]
    (if (< x y)
      (map (partial util/matrix-add [x0 y0]) (apply interleave acc))
      (let [points [[x y] [y x] [(* -1 x) y] [(* -1 y) x]
                    [(* -1 x) (* -1 y)] [(* -1 y) (* -1 x)]
                    [x (* -1 y)] [y (* -1 x)]]
            y (inc y)]
        (if (neg? err)
          (recur x y (inc (* 2 y)) (conj acc points))
          (recur (dec x) y (* 2 (inc (- y x))) (conj acc points)))))))

(defn visible-lines
  "Collection of all possible lines from `[x0 y0]` to a circle around
  that point with radius `r` with precision 0.5. Duplicates not removed."
  [[x0 y0] r]
  (let [turn (* Math/PI 2)
        segments (* 2 (Math/ceil (* r turn)))]
    (for [rad (range 0 turn (/ turn segments))]
      (for [r (range 0 (+ r 0.5) 0.5)]
        (util/matrix-add [x0 y0]
                         [(Math/round (* r (Math/cos rad)))
                          (Math/round (* r (Math/sin rad)))])))))

(defn- octant-projection
  "Determine parameters for a projection of a bresenham line from
  `[0 0]` to `[x y]` in the first octant.

  Evaluates to a vector of three values:
  1. Positive length of the longer, i.e. quicker axis to iterate along
  2. Gradient for the bresenham line in the first octant
  3. Projection function for input values `[x y]` that maps these back
     to the correct octant of the original input parameter.

  Also covers the four special cases of projections along the x and y
  axes in both directions, i.e. when either `x` or `y` is zero.
  `x` and `y` must not be both zero."
  [[x y]]
  {:pre [(not (every? zero? [x y]))]}
  (let [x-side (cond (zero? x) :none
                     (neg? x) :left
                     :else :right)
        y-side (cond (zero? y) :none
                     (neg? y) :top
                     :else :bottom)
        m (if (some zero? [x y]) 0 (float (/ y x)))
        side (cond (zero? m) :none
                   (<= (Math/abs m) 1) :inner
                   :else :outer)]
    (case [side x-side y-side]
      [:none :right :none] [x 0 (fn [[x y]] [x y])]
      [:none :left :none] [(* -1 x) 0 (fn [[x y]] [(* -1 x) y])]
      [:none :none :bottom] [y 0 (fn [[x y]] [y x])]
      [:none :none :top] [(* -1 y) 0 (fn [[x y]] [y (* -1 x)])]
      [:inner :right :bottom] [x m (fn [[x y]] [x y])]
      [:outer :right :bottom] [y (/ 1 m) (fn [[x y]] [y x])]
      [:outer :left :bottom] [y (/ -1 m) (fn [[x y]] [(* -1 y) x])]
      [:inner :left :bottom] [(* -1 x) (* -1 m) (fn [[x y]] [x (* -1 y)])]
      [:inner :left :top] [(* -1 x) m (fn [[x y]] [(* -1 x) (* -1 y)])]
      [:outer :left :top] [(* -1 y) (/ 1 m) (fn [[x y]] [(* -1 y) (* -1 x)])]
      [:outer :right :top] [(* -1 y) (/ -1 m) (fn [[x y]] [y (* -1 x)])]
      [:inner :right :top] [x (* -1 m) (fn [[x y]] [(* -1 x) y])])))

(defn bresenham-line
  "Sequence of points from `[x0 y0]` to `[x1 y1]` determined using the
  Bresenham algorithm"
  [[x0 y0] [x1 y1]]
  (let [[dx dy] (util/matrix-subtract [x1 y1] [x0 y0])
        [n m projectf] (octant-projection [dx dy])
        err (fn [x] (- (* x m) 0.5))]
    (for [quick (range 0 (inc n)) :let [slow (int (Math/ceil (err quick)))]]
      (util/matrix-add [x0 y0] (projectf [quick slow])))))

(comment
  ; Useful for playing around
  (defn test-algebra [coll]
    (screen/in-screen screen
      (loop [targets (cycle coll)]
        (let [[name coords] (first targets)]
          (screen/clear screen)
          (graphics/put graphics (str name) 0 0)
          (doseq [[x y] coords]
            (graphics/put graphics "x" x y))
          (screen/refresh screen)
          (when (not= (:key (screen/read-input-blocking screen)) :escape)
            (recur (next targets)))))))

  (test-algebra (partition 2 (interleave (iterate inc 0) (algebra/visible-lines [40 12] 8))))

  (test-algebra [["radius" (algebra/visible-radius [40 12] 8)] ["area" (algebra/visible-area [40 12] 8)]]))
