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

(ns statehack.system.inventory
  (:require [statehack.component :as c]
            [statehack.component.menu :as cm]
            [statehack.entity :as entity]
            [statehack.entity.menu :as menu]
            [statehack.system.input :as input]
            [statehack.system.input.receivers :as receivers]
            [statehack.system.messages :as messages]
            [statehack.system.name :as name]
            [statehack.system.position :as pos]
            [statehack.system.slots :as slots]
            [statehack.system.world :as world]))

(def default-frame {:pickup :floor
                    :inventory :inventory})

(defn in-inventory? [e1 e2]
  {:pre [(::c/inventory e1) (::c/pickup e2)]}
  (some #{(::c/id e2)} (::c/inventory e1)))

(defn pick-up-item [game e1 e2]
  {:pre [(::c/inventory e1) (::c/pickup e2)]}
  (world/update game [{actor-id ::c/id} (::c/id e1) {item-id ::c/id} (::c/id e2)]
    (world/update-entity-component game actor-id ::c/inventory conj item-id)
    (world/remove-entity-component game item-id ::c/position ::c/floor)))

(defn drop-item [game e1 e2]
  {:pre [(in-inventory? e1 e2)]}
  (world/update game [{actor-id ::c/id :keys [::c/floor ::c/position] :as actor} (::c/id e1)
                      {item-id ::c/id :as item} (::c/id e2)]
    (slots/unslot game actor item)
    (world/update-entity-component game actor-id ::c/inventory (partial remove #{item-id}))
    (world/add-entity-component game item-id ::c/position position ::c/floor floor)))

(defn available-pickups [game e]
  (entity/filter-capable [::c/pickup] (pos/entities-at game e)))

(defn- frame-items [game e frame]
  (if (= frame :floor)
    (available-pickups game e)
    (map (partial world/entity game) (::c/inventory e))))

(defn change-index [game {:keys [::cm/inventory ::c/id] :as menu} f]
  (let [{:keys [::cm/reference ::cm/index ::cm/frame]} inventory
        player (world/entity game reference)
        max (count (frame-items game player frame))]
    (world/update-in-entity-component game id ::cm/inventory [::cm/index]
      (comp (if (zero? max) (constantly 0) #(mod % max)) f))))

(defn pick-up-or-drop [game {:keys [::cm/inventory] :as menu}]
  (let [{:keys [::cm/reference ::cm/index ::cm/frame]} inventory
        actor (world/entity game reference)
        items (frame-items game actor frame)]
    (if (empty? items)
      game
      (let [f (if (= frame :floor) pick-up-item drop-item)]
        (-> game (f actor (nth items index)) (change-index menu identity))))))

(defmulti activate
  (fn [game actor item]
    {:pre [(entity/capable? item ::c/pickup)]}
    (::c/pickup item)))

(defmethod activate :none [game _ item]
  (messages/log game (str "Don't know how to activate " (name/name item))))

(defmethod activate :slot-weapon [game actor item]
  (let [{:keys [::c/weapon]} item
        {:keys [::c/type]} weapon
        {:keys [::c/slots]} actor]
    (if ((slots/available-slots actor) type)
      (slots/slot game actor item type)
      (messages/log game (format "%s cannot use %s" (name/name actor) (name/name item))))))

(defn activate-item [game {:keys [::cm/inventory] :as menu}]
  (let [{:keys [::cm/reference ::cm/index ::cm/frame]} inventory
        actor (world/entity game reference)
        items (frame-items game actor frame)]
    (if (empty? items)
      game
      (activate game actor (nth items index)))))

(defn inventory-type
  "The type of inventory-menu

  If applicable."
  [menu]
  (get-in menu [::cm/inventory ::cm/type]))

(defmulti change-frame
  (fn [_ menu direction]
    [(inventory-type menu) direction]))

(defmethod change-frame :default [game _ _] game)

(defn- change-frame-common [game menu frame]
  (world/update game [{:keys [::c/id] :as menu} (::c/id menu)]
    (world/update-in-entity-component game id ::cm/inventory [::cm/frame] (constantly frame))
    (change-index game menu identity)))

(defmethod change-frame [:pickup :left] [game menu _]
  (change-frame-common game menu :floor))

(defmethod change-frame [:pickup :right] [game menu _]
  (change-frame-common game menu :inventory))

(defn inventory-open?
  "Is the current input receiver an `inventory` menu?"
  [game]
  (entity/capable? (receivers/current game) ::cm/inventory))

(defmulti handle-enter
  "Handle the `enter` key

  Depends on the type of `inventory`."
  (fn [_ menu] (inventory-type menu)))

(defmethod handle-enter :pickup [game menu]
  (pick-up-or-drop game menu))

(defmethod handle-enter :inventory [game menu]
  (activate-item game menu))

(defn open [game player type]
  (let [i (menu/inventory (::c/id player) type (default-frame type))]
    (world/update game [] (world/add-entity game i) (receivers/push-control game i))))

(defmethod input/receive :inventory-menu [game menu input]
  (case (:key input)
    :escape (world/update game []
              (receivers/pop-control game)
              (world/remove-entity game (::c/id menu)))
    \w (change-index game menu inc)
    \x (change-index game menu dec)
    \a (change-frame game menu :left)
    \d (change-frame game menu :right)
    :enter (handle-enter game menu)
    game))
