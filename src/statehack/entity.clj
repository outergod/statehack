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
  (:require [clojure.pprint :refer [cl-format]]
            [clojure.spec :as s]
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

;;; Entities

(defn conform
  "Conform `x` to `:statehack/entity`"
  [x]
  (let [conformed (s/conform :statehack/entity x)]
    (if (= conformed ::s/invalid)
      (throw
        (ex-info (cl-format nil "~a fails to validate~%~s" x (s/explain-str :statehack/entity x))
          (s/explain-data :statehack/entity x)))
      x)))

(defn cursor
  "Cursor entity"
  []
  (conform #::c{:id (uuid) :unique :cursor :position [0 0] :mobile :cursor}))

(defn dialog
  "Dialog entity"
  [& ms]
  (conform #::c{:id (uuid) :unique :dialog :renderable :dialog :input :dialog :messages ms}))

(defn floor
  "Floor entity"
  [n [w h]]
  (conform #::c{:id (uuid) :renderable :floor :floor n :foundation [w h]}))

(defn log
  "Log entity"
  []
  (conform #::c{:id (uuid) :unique :log :renderable :log :messages []}))

(defn music
  "Music entity"
  [name]
  (conform #::c{:id (uuid) :music name}))

(defn player
  "Player entity"
  [name hp]
  (conform
    #::c{:id (uuid)
         :name name
         :category :human
         :renderable :humanoid
         :input :player
         :alive true
         :unique :player
         :obstacle :always
         :memory {}
         :inventory []
         :slots {:melee nil :gun nil}
         :mobile :bipedal
         :adaptive #::c{:xp 0}
         :vulnerable #::c{:hp hp :max hp}
         :sight #::c{:type :eyes :distance 10}}))

(defn selector
  "Cursor-based selector

  Uses deferred `action` on selected target."
  [[x y] action targets]
  (conform #::c{:id (uuid) :position [x y] :input :selector :deferred action
                :mobile :selector :selector targets}))

(defn serv-bot
  "Serv-Bot entity"
  []
  (conform
    #::c{:id (uuid)
         :category :serv-bot
         :renderable :serv-bot
         :alive true
         :obstacle :always
         :memory {}
         :inventory []
         :ai :serv-bot
         :armor 20
         :mobile :wheels
         :vulnerable #::c{:hp 20 :max 20}
         :sight #::c{:type :sensors :distance 5}
         :skillset {:melee #::c{:id (uuid)
                                :name "Appendages"
                                :weapon #::c{:type :melee :transition :appendages
                                             :damage 8 :penetration 0 :offense 1}}}}))

(defn status-bar
  "Status bar entity"
  []
  (conform #::c{:id (uuid) :renderable :status}))
