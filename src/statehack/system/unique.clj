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

(ns statehack.system.unique
  (:require [statehack.system.world :as world]))

(defn unique-entity [game type]
  (let [es (filter #(= (:unique %) type) (world/capable-entities game :unique))]
    (case (count es)
      0 nil
      1 (first es)
      (throw (ex-info (format "Found %d entities satisfying %s, expected exactly one" (count es) type) {})))))
