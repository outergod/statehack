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

(ns halo.screen
  (:require [halo.terminal :as terminal]
            [halo.util :as util])
  (:import [com.googlecode.lanterna TerminalSize TerminalPosition]
           [com.googlecode.lanterna.screen DefaultScreen Screen]
           [com.googlecode.lanterna.graphics TextGraphics]))

(defn screen [& opts]
  (DefaultScreen. (apply terminal/terminal opts)))

(defn start
  [^Screen screen]
  (.startScreen screen))

(defn stop
  [^Screen screen]
  (.stopScreen screen))

(defmacro in-screen
  [screen & body]
  `(let [screen# ~screen]
     (start screen#)
     (try ~@body
          (finally (stop screen#)))))

(defn- extract-size
  [^TerminalSize size]
  [(.getColumns size) (.getRows size)])

(defn size
  [^Screen screen]
  (extract-size (.getTerminalSize screen)))

(defn refresh
  [^Screen screen]
  (.refresh screen))

(defn move-cursor
  [^Screen screen [x y]]
  (.setCursorPosition screen (TerminalPosition. x y)))

(defn hide-cursor
  [^Screen screen]
  (.setCursorPosition screen nil))

(defn clear
  [^Screen screen]
  (.clear screen))

(defn read-input
  [^Screen screen]
  (util/translate-stroke (.readInput screen)))

(defn read-input-blocking
  [screen]
  (util/wait-for #(read-input screen)))

(defn text-graphics
  [^Screen screen]
  (.newTextGraphics screen))

(defn probe-resize
  [^Screen screen]
  (when-let [size (.doResizeIfNecessary screen)]
    (extract-size size)))
