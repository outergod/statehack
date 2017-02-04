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

(ns statehack.component.menu
  "statehack menu components"
  (:require [clojure.spec :as s]
            [statehack.component :as c]
            [statehack.util :as util]))

;;; Inventory menu component
;;;
;;; References other entity `id`"
;;;
;;; Related systems: inventory
(s/def ::index util/pos-int-or-zero?)
(s/def ::frame keyword?)
(s/def ::type keyword?)
(s/def ::reference ::c/id)
(s/def ::inventory
  (s/keys :req [::index ::frame
                ::type  ::reference]))
