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

(ns statehack.system.combat
  (:require [clojure.pprint :refer [cl-format]]
            [statehack.component :as c]
            [statehack.entity :as entity]
            [statehack.system.messages :as messages]
            [statehack.system.name :as name]
            [statehack.system.position :as pos]
            [statehack.system.skills :as skills]
            [statehack.system.slots :as slots]
            [statehack.system.transition :as transition]
            [statehack.system.world :as world]))

(defmulti hurt (fn [game actor] (::c/category actor)))

(defmethod hurt :default [game _] game)

(defmethod hurt :human [game _]
  (transition/transition game (transition/sound :player-hurt)))

(defmulti die (fn [game actor] (::c/category actor)))

(defmethod die :default [game _] game)

(defmethod die :serv-bot [game _]
  (transition/transition game (transition/sound :bot-die)))

(defn target-hp [e]
  (get-in e [::c/vulnerable ::c/hp]))

(defn die-common [game e]
  (world/update game [{:keys [::c/id] :as e} (::c/id e)]
    (world/remove-entity-component game id ::c/mobile ::c/obstacle ::c/ai)
    (world/update-entity-component game id ::c/alive (constantly false))
    (world/update-entity-component game id ::c/renderable (constantly :corpse))
    (die game e)))

(defn dead? [e]
  (not (::c/alive e)))

(defn- reduction [weapon target]
  (max (- (or (::c/armor target) 0)
          (:penetration weapon))
       0))

(defn damage [game attacker {:keys [::c/weapon] :as item} target]
  (let [{:keys [::c/damage ::c/offense]} weapon
        armor (or (::c/armor target) 0)
        hp (- (target-hp target) damage)
        game (-> game
                 (messages/log (cl-format nil "~a attacks ~a with ~a, causing ~d damage" (name/name attacker) (name/name target) (name/name item) damage))
                 (hurt target))]
    (if (pos? hp)
      (world/update-entity-component game (::c/id target) [::c/vulnerable ::c/hp] - damage)
      (world/update game [target (::c/id target)]
        (messages/log game (cl-format nil "~a dies from ~d overdamage" (name/name target) (Math/abs hp)))
        (die-common game target)))))

(defn attackable? [e]
  (and (entity/capable? e ::c/vulnerable)
       (::c/alive e)))

(defn melee-dispatch [game attacker {:keys [::c/weapon]} target]
  (:transition weapon))

(defmulti melee #'melee-dispatch)

(defmethod melee :default [game attacker item target]
  (damage game attacker item target))

(defmethod melee :appendages [game attacker item target]
  (-> game
      (transition/transition (transition/sound :appendage-attack))
      (damage attacker item target)))

(defmethod melee :lead-pipe [game attacker item target]
  (-> game
      (transition/transition (transition/sound :lead-pipe-hit))
      (damage attacker item target)))

(defn available-melee [game e]
  (if-let [w (or (slots/slot-item game e :melee)
                 (skills/skill e :melee))]
    (let [es (filter attackable? (pos/entity-neighbors game e))]
      (into {} (map (fn [t] [(pos/entity-delta t e) #(melee % e w t)]) es)))
    {}))
