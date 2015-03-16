;;;; This file is part of statehack.
;;;;
;;;; Copyright © 2015 Alexander Kahl
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

(ns statehack.dev.benchmark)

(defmacro record-time [binding time-expr expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr
         ~binding (/ (double (- (. System (nanoTime)) start#)) 1000000.0)]
     ~time-expr
     ret#))
