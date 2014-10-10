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
  [^Screen screen x y]
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
