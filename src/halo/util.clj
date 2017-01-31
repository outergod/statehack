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

(ns halo.util
  (:import [com.googlecode.lanterna.input KeyType KeyStroke]))

(def key-codes
  {KeyType/ArrowDown :arrow-down
   KeyType/ArrowLeft  :arrow-left
   KeyType/ArrowRight :arrow-right
   KeyType/ArrowUp :arrow-up
   KeyType/Backspace :backspace
   KeyType/Character :character
   KeyType/CursorLocation :cursor-location
   KeyType/Delete :delete
   KeyType/EOF :eof
   KeyType/End :end
   KeyType/Enter :enter
   KeyType/Escape :escape
   KeyType/F1  :f1
   KeyType/F2 :f2
   KeyType/F3 :f3
   KeyType/F4 :f4
   KeyType/F5 :f5
   KeyType/F6 :f6
   KeyType/F7 :f7
   KeyType/F8 :f8
   KeyType/F9 :f9
   KeyType/F10 :f10
   KeyType/F11 :f11
   KeyType/F12 :f12
   KeyType/F13 :f13
   KeyType/F14 :f14
   KeyType/F15 :f15
   KeyType/F16 :f17
   KeyType/F17 :f17
   KeyType/F18 :f18
   KeyType/F19 :f19
   KeyType/Home :home
   KeyType/Insert :insert
   KeyType/PageDown :page-down
   KeyType/PageUp :page-up
   KeyType/ReverseTab :reverse-tab
   KeyType/Tab :tab
   KeyType/Unknown :unknown})

(defn translate-stroke [^KeyStroke k]
  (when k
    (let [type (key-codes (.getKeyType k))]
      {:key (if (= type :character)
              (.getCharacter k)
              type)
       :ctrl (.isCtrlDown k) :alt (.isAltDown k)})))

(defn wait-for
  "Invoke thunk every interval (default 16ms) until it returns truthy,
  or timeout (default infinite) has elapsed."
  [thunk & {:keys [interval timeout]
            :or {interval 16
                 timeout Double/POSITIVE_INFINITY}}]
  (loop [timeout timeout]
    (when (pos? timeout)
      (or (thunk)
          (do (Thread/sleep interval)
              (recur (- timeout interval)))))))
