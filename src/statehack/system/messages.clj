(ns statehack.system.messages
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

(defn recent [e n]
  {:pre [(pos? n)]}
  (letfn [(format [[s n]]
            (cl-format false "~a~:[~; [repeated ~d times]~]" s (> n 1) n))]
    (map format
         (reverse
          (reduce (fn [[[last n] & acc] current]
                    (cond (= current last) (conj acc [last (inc n)])
                          last (conj acc [last n] [current 1])
                          :default [[current 1]]))
                  nil (take n (:messages e)))))))

(defn pop [game e]
  (world/update-entity-component game e :messages next))

(defn push [game e & ms]
  (world/update-entity-component game e :messages (partial concat ms)))

(defn log [game s]
  (push game (unique/unique-entity game :log) s))
