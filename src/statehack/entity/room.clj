(ns statehack.entity.room
  (:require [statehack.entity :refer :all]
            [statehack.component :as c]
            [clojure.string :as str]
            [statehack.util :as util]))

(defn wall [x y]
  (entity
   (c/renderable :wall)
   (c/position x y)
   (c/room)
   (c/obstacle)))

(defn door [x y open?]
  (entity
   (c/renderable :door)
   (c/position x y)
   (c/door open?)
   (c/room)
   (c/obstacle)))

(defn extract-room [s x0 y0]
   (let [lines (str/split-lines s)
         find-wall (memoize (fn [[x y]] (#{\X \O \o} (get-in lines [y x]))))]
     (filter identity
             (flatten
              (for [[row y] (partition 2 (interleave lines (range (count s))))
                    [token x] (partition 2 (interleave row (range (count row))))]
                (when-let [w (find-wall [x y])]
                  (let [[x1 y1] (util/matrix-add [x0 y0] [x y])]
                    (case w
                      \X (wall x1 y1)
                      \O (door x1 y1 true)
                      \o (door x1 y1 false)))))))))
