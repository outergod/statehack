(ns statehack.system.levels
  (:refer-clojure :exclude [load])
  (:require [statehack.entity :as entity]
            [statehack.entity.room :as room]
            [statehack.util :as util]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn load-resource [name]
  (or (io/resource (str "levels/" name))
      (throw (ex-info (str "No such level resource found") {:name name}))))

(defn extract [s [x0 y0]]
   (let [lines (str/split-lines s)
         find-wall (memoize (fn [[x y]] (#{\X \O \o \e} (get-in lines [y x]))))]
     (filter identity
             (flatten
              (for [[row y] (partition 2 (interleave lines (range (count s))))
                    [token x] (partition 2 (interleave row (range (count row))))]
                (when-let [w (find-wall [x y])]
                  (let [[x1 y1] (util/matrix-add [x0 y0] [x y])]
                    (case w
                      \X (room/wall x1 y1)
                      \O (room/door x1 y1 true)
                      \o (room/door x1 y1 false)
                      \e (room/solid x1 y1)))))))))

(defn load [name]
  (extract (slurp (load-resource name)) [0 0]))

(defn dimensions [level]
  (let [ps (map :position (entity/filter-capable [:position] level))]
    [(inc (apply max (map first ps)))
     (inc (apply max (map second ps)))]))
