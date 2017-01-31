;;;; This file is part of statehack.
;;;;
;;;; statehack is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; statehack is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with statehack.  If not, see <http://www.gnu.org/licenses/>.

(ns statehack.algebra
  "Algebraic algorithms for rasterized, linear planes"
  (:require [clojure.set :as set]
            [clojure.data.priority-map :refer [priority-map-keyfn]]
            [statehack.util :as util])
  (:import [java.util Comparator]))

(def neighbor-deltas
  "Set of neighbor node deltas"
  (set (remove #(= % [0 0])
               (for [x [-1 0 1] y [-1 0 1]] [x y]))))

(def neighbors
  "Set of `[x y]`'s neighbors

  Memoized."
  (memoize
   (fn [[x y]]
     (set (map (partial util/matrix-add [x y]) neighbor-deltas)))))

(defn euclidian-distance
  "Euclidian distance between two coordinates"
  ([[x y]] (Math/sqrt (+ (Math/pow x 2) (Math/pow y 2))))
  ([[x0 y0] [x1 y1]] (euclidian-distance [(- x1 x0) (- y1 y0)])))

(defn- filter-circle
  "Evaluate to all points in a square with length `(* r 2)` around
  `[x0 y0]` that satisfy distance predicate `pred` so that 
  `(pred distance-to-x0-y0 r)` is true."
  [[x0 y0] r pred]
  (let [start (* r -1) end (inc r)]
    (map (partial util/matrix-add [x0 y0])
         (filter (fn [[x y]]
                   (pred (Math/round (euclidian-distance [x y])) r))
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

(defn- visible-lines-origin
  "Collection of all distinct possible lines from `[0 0]` to a circle around
  that point with radius `r`."
  [r]
  (let [turn (* Math/PI 2)
        segments (Math/ceil (* r r turn))]
    (distinct
      (for [rad (range 0 turn (/ turn segments))]
        (distinct
          (for [r (range 1 (inc r))]
            [(Math/round (* r (Math/cos rad)))
             (Math/round (* r (Math/sin rad)))]))))))

(def visible-lines-seq
  "Infinite lazy sequence of visible lines around `[0 0]`"
  (map visible-lines-origin (iterate inc 1)))

;; Calculate the first ten radii around `[0 0]`
(take 10 visible-lines-seq)

(defn visible-lines
  "Collection of all possible lines from `[x0 y0]` to a circle around
  that point with radius `r`."
  [[x0 y0] r]
  (for [line (nth visible-lines-seq (dec r))]
    (map (partial util/matrix-add [x0 y0]) line)))

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
  (if (= [x0 y0] [x1 y1])
    [[x0 y0]]
    (let [[dx dy] (util/matrix-subtract [x1 y1] [x0 y0])
          [n m projectf] (octant-projection [dx dy])
          err (fn [x] (- (* x m) 0.5))]
      (for [quick (range 0 (inc n)) :let [slow (int (Math/ceil (err quick)))]]
        (util/matrix-add [x0 y0] (projectf [quick slow]))))))

(defn frame
  "Calculate set of coordinates framing an area of boundaries `[w h]`"
  [[w h]]
  (set/union (set (mapcat (fn [x] [[x -1] [x h]]) (range -1 (inc w))))
             (set (mapcat (fn [y] [[-1 y] [w y]]) (range -1 (inc h))))))

(defn- a*-seq
  "Lazy sequence of A* iterations"
  [[x0 y0] [x1 y1] [w h] os]
  (let [os (set/union os (frame [w h]))]
    (if (os [x1 y1])
      nil
      (letfn [(h [[x y]] (euclidian-distance [x y] [x1 y1]))
              (f [[x y] g] (+ g (h [x y])))
              (n [[x y] g parents fringe closed]
                (let [delta (fn [[x1 y1]]
                              (+ g (if (or (= x1 x) (= y1 y)) 1 1.5)))
                      parents (conj parents [x y])]
                  (keep (fn [[x y]]
                          (let [g (delta [x y])
                                cost (f [x y] g)
                                [prev-cost] (fringe [x y])]
                            (when-not (and prev-cost (<= prev-cost cost))
                              [[x y] [cost g parents]])))
                        (set/difference (neighbors [x y]) os closed))))]
        (iterate
          (fn [[state fringe closed goal]]
            (if (= state :stop)
              [state fringe closed goal]
              (let [[[x y] [cost distance parents] :as node] (peek fringe)
                   [[x-goal y-goal] [cost-goal _ parents-goal] :as node-goal] (peek goal)]
               (cond (= [x y] [x1 y1]) [:continue (pop fringe) closed (conj goal node)]
                     (and (not-empty node-goal)
                       (or (empty? node)
                         (< cost-goal cost))) [:stop (conj parents-goal [x-goal y-goal])]
                     node [:continue (into (pop fringe) (n [x y] distance parents fringe closed)) (conj closed [x y]) goal]))))
          [:continue
           (priority-map-keyfn first [x0 y0] [(h [x0 y0]) 0 []])
           #{}
           (priority-map-keyfn first)])))))

(defn a*
  [[x0 y0] [x1 y1] [w h] os limit]
  (let [[state path] (last (take limit (a*-seq [x0 y0] [x1 y1] [w h] os)))]
    (if (= state :stop) path nil)))

(defn path-shortcuts
  "Count the number of diagonal moves in `path`

  This is used as a secondary length metric for paths that have the
  same number of steps."
  [path]
  (first (reduce (fn [[c [x0 y0]] [x1 y1]]
                   [(if (or (= x0 x1) (= y0 y1)) c (inc c))
                    [x1 y1]])
                 [0 (first path)] (next path))))

(def PathComparator
  "Comparator for the length of two paths

   Takes the number of shortcuts into account"
  (reify Comparator
    (compare [_ path1 path2]
      (let [c1 (count path1) c2 (count path2)
            s1 (path-shortcuts path1) s2 (path-shortcuts path2)]
        (cond (= c1 c2) (cond (= s1 s2) 0
                              (< s1 s2) -1
                              :default 1) 
              (< c1 c2) -1
              :default 1)))))

(comment
  ; Useful for playing around
  (defn test-algebra [coll [x0 y0]]
    (screen/in-screen screen
      (loop [targets (cycle coll)]
        (let [[name coords] (first targets)]
          (screen/clear screen)
          (graphics/put graphics (str name) 0 0)
          (graphics/put graphics "@" x0 y0)
          (doseq [[x y] coords]
            (graphics/put graphics "x" x y))
          (screen/refresh screen)
          (when (not= (:key (screen/read-input-blocking screen)) :escape)
            (recur (next targets)))))))

  (test-algebra (partition 2 (interleave (iterate inc 0) (algebra/visible-lines [40 12] 8))) [40 12])

  (test-algebra [["radius" (algebra/visible-radius [40 12] 8)] ["area" (algebra/visible-area [40 12] 8)]] [40 12]))
