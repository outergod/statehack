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

(ns statehack.system.obstacle
  "Obstacle facility"
  (:require [statehack.system.door :as door]))

(def obstacle?-hierarchy "Hierarchy for `obstacle?`" (make-hierarchy))

(defn derive-obstacle?
  "Derive for `obstacle?-hierarchy`"
  [tag parent]
  (alter-var-root #'obstacle?-hierarchy derive tag parent))

(def obstacle?-dispatch
  "Dispatch for `obstacle?`"
  :obstacle)

(defmulti obstacle?
  "Is entity an obstacle?"
  {:arglists '([e])}
  #'obstacle?-dispatch :hierarchy #'obstacle?-hierarchy)

(defmethod obstacle? :default [_] true)
(defmethod obstacle? nil [_] false)
(defmethod obstacle? :door [e] (not (door/open? e)))

(defn filter-obstacles [es]
  (filter obstacle? es))
