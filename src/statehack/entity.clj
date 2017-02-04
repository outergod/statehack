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

(ns statehack.entity
  "Entities are the most basic units in ECS"
  (:require [clojure.spec :as s]
            [clojure.set :as set]
            [statehack.component :as c]))

(defn uuid
  "Generate a new, random UUIDv4"
  []
  (java.util.UUID/randomUUID))

(s/def :statehack/entity
  (s/keys
    :req [::c/id]
    :opt [::c/unique ::c/name ::c/position ::c/category ::c/vulnerable ::c/armor
          ::c/alive ::c/adaptive ::c/skillset ::c/obstacle ::c/opaque ::c/sight
          ::c/door ::c/room ::c/input ::c/mobile ::c/floor ::c/foundation
          ::c/renderable ::c/color ::c/messages ::c/deferred ::c/ai ::c/memory
          ::c/inventory ::c/pickup ::c/music ::c/slots ::c/weapon ::c/compound
          ::c/label]))

(defn components
  "Set of all components of entity"
  [e]
  (set (keys e)))

(defn capable?
  "Does entity have all given components?"
  [e & cs]
  (set/subset? (set cs) (components e)))

(defn filter-capable
  "Filter entities that have all given components"
  [[& cs] es]
  (filter #(apply capable? % cs) es))

(defn remove-capable
  "Remove entities that have all given components"
  [[& cs] es]
  (remove #(apply capable? % cs) es))

