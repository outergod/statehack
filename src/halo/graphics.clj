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

(defn put
  ([^TextGraphics graphics s x y]
     (.putString graphics x y s))
  ([^TextGraphics graphics s x y & {:keys [color]}]
     (when color
       (if (sequential? color)
         (apply set-color graphics color)
         (set-color graphics color)))
     (put graphics s x y)))
