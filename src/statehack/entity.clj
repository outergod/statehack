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

(ns statehack.entity
  (:require [clojure.set :as set]))

(defn uuid [] (java.util.UUID/randomUUID))

(defn entity [& components]
  (apply merge {:id (uuid)} components))

(defn components [e]
  (set (keys e)))

(defn capable? [e & cs]
  (set/subset? (set cs) (components e)))

(defn filter-capable [[& cs] es]
  (filter #(apply capable? % cs) es))

(defn remove-capable [[& cs] es]
  (remove #(apply capable? % cs) es))

