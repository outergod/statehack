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

(ns halo.graphics
  (:require [halo.screen :as screen])
  (:import [com.googlecode.lanterna TerminalSize TextColor$RGB TextColor$Indexed]
           [com.googlecode.lanterna.graphics TextGraphics]))

(defn color
  ([r g b] (TextColor$RGB. r g b))
  ([i] (TextColor$Indexed. i)))

(defn size
  [^TextGraphics graphics]
  (let [^TerminalSize size (.getSize graphics)]
    [(.getColumns size) (.getRows size)]))

(defn set-color
  ([^TextGraphics graphics r g b]
     (.setForegroundColor graphics (color r g b)))
  ([^TextGraphics graphics i]
     (.setForegroundColor graphics (color i))))

(defn set-background-color
  ([^TextGraphics graphics r g b]
     (.setBackgroundColor graphics (color r g b)))
  ([^TextGraphics graphics i]
     (.setBackgroundColor graphics (color i))))

;; Type hints are important, performance-sensitive spot
(defn put
  ([^TextGraphics graphics ^String s ^Integer x ^Integer y]
   (.putString graphics x y s))
  ([^TextGraphics graphics s x y & {:keys [color background]}]
   (doseq [[c f] [[color set-color] [background set-background-color]]]
     (when c
       (if (sequential? c)
         (apply f graphics c)
         (f graphics c))))
     (put graphics s x y)))
