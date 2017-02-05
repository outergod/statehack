;;;; This file is part of statehack.
;;;;
;;;; Copyright © 2014-2017 Alexander Kahl <ak@sodosopa.io>
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

(ns statehack.system.messages
  (:refer-clojure :exclude [update pop])
  (:require [clojure.pprint :refer [cl-format]]
            [statehack.component :as c]
            [statehack.entity :as entity]
            [statehack.system.input.receivers :as receivers]
            [statehack.system.unique :as unique]
            [statehack.system.world :as world]))

(defn dialog [game & ms]
  (let [d (apply entity/dialog ms)]
    (world/update game []
      (world/add-entity game d)
      (receivers/push-control game d))))

(defn current [e]
  (first (::c/messages e)))

(defn update [game e f]
  (world/update-entity-component game (::c/id e) ::c/messages f))

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
       (take n (::c/messages e))))
