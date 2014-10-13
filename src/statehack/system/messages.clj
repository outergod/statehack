(ns statehack.system.messages
  (:refer-clojure :exclude [pop])
  (:require [statehack.entity.dialog :as dialog]
            [statehack.system.input.receivers :as receivers]
            [statehack.system.world :as world]
            [statehack.system.unique :as unique]
            [clojure.pprint :refer [cl-format]]))

(defn dialog [game & ms]
  (let [d (apply dialog/dialog ms)]
    (-> game (world/add-entity d) (receivers/push-control d))))

(defn current [e]
  (first (:messages e)))

(defn update [game e f]
  (world/update-entity-component game e :messages f))

(defn pop [game e]
  (update game e next))

(defn push [game e & ms]
  (update game e (partial concat ms)))

(defn log [game s]
  (let [e (unique/unique-entity game :log)
        [last n] (current e)]
    (if (= s last)
      (update game e (fn [[_ & ms]] (conj ms [last (inc n)])))
      (push game e [s 1]))))

(defn recent [e n]
  {:pre [(pos? n)]}
  (map (fn [[s n]]
         (cl-format false "~a~:[~; [repeated ~d times]~]" s (> n 1) n))
       (take n (:messages e))))
