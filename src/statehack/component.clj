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

(ns statehack.component
  "All statehack components"
  (:require [clojure.spec :as s]
            [statehack.util :as util]))

(s/def ::type keyword?)
(s/def ::pos-int-or-zero (s/and int? util/pos-int-or-zero?))
(s/def ::distance ::pos-int-or-zero)

;;; ID component
;;;
;;; A unique identifier is the bare minimum for any entity to exist.
;;;
;;; Related systems: all
(s/def ::id uuid?)

;;; Unique entity component
;;; 
;;; Every type must only exist at most once among all entities.
;;;
;;; Related systems: unique
(s/def ::unique keyword?)

;;; Name component
;;;
;;; Gives an entity a name.
;;;
;;; Related systems: name, status
(s/def ::name string?)

;;; Position component
;;; 
;;; Entities with this component exist on a linear plane at the current
;;; coordinates. This does not imply they are visible or can't coexist
;;; at the same location.
;;;
;;; Related systems: movement, levels, defer, render, sight, viewport, world
(s/def ::coordinate ::pos-int-or-zero)
(s/def ::position (s/tuple ::coordinate ::coordinate))

;;; Category component
;;;
;;; Simply used to further specify the type of an entity to influence
;;; rendering and, possibly, other systems.
;;;
;;; Related systems: name
(s/def ::category keyword?)

;;; Vulnerability component
;;;
;;; A vulnerable entity can be damaged and has a notion of health.
;;;
;;; Related systems: combat, status
(s/def ::hp ::pos-int-or-zero)
(s/def ::max pos-int?)
(s/def ::vulnerable (s/keys :req [::hp ::max]))

;;; Armor component
;;;
;;; Armor reduces damage done in combat.
;;;
;;; Related systems: combat
(s/def ::armor pos-int?)

;;; Life component
;;;
;;; Determines whether an entity can be any of the two states of alive
;;; and dead.
;;;
;;; Related systems: combat
(s/def ::alive boolean?)

;;; Adaptivity component
;;;
;;; An adaptive component learns by accomplishing things and can improve
;;; upon its experience.
;;;
;;; Related systems: status
(s/def ::xp ::pos-int-or-zero)
(comment
  "TODO Replace me"
  (s/def ::level ::pos-int-or-zero)) 
(s/def ::adaptive (s/keys :req [::xp]))

;;; Skillset component
;;;
;;; A skilled entity can perform actions other than movement.
;;;
;;; Related systems: skills, combat
(s/def ::skillset (s/every-kv keyword? :statehack/entity))

;;; Obstacle component
;;;
;;; Entities that are also obstacles can't co-exist at the same
;;; position, i.e. they obstruct each other. Typical obstacles include
;;; walls, closed doors, force fields, enemies, the player. 
;;;
;;; Related systems: obstacle"
(s/def ::obstacle keyword?)

;;; Opacity component
;;;
;;; Opaque entities can simply not be looked through, i.e. they obstruct the line
;;; of view. Can but doesn't need to be combined with `obstacle`.
;;;
;;; Related systems: sight
(s/def ::opaque keyword?)

;;; Sight component
;;;
;;; Seeing entities can detect the presence or absence of other entities
;;; within `distance`, as dispatch on `type` determines.
;;;
;;; Related systems: sight, memory, render
(s/def ::sight (s/keys :req [::type ::distance]))

;;; Door component
;;;
;;; A door can be open or closed and behave differently depending on that.
;;;
;;; Related systems: door, obstacle, levels, render
(s/def ::open? boolean?)
(s/def ::door (s/keys :req [::type ::open?]))

;;; Room component
;;;
;;; Entities which are considered part of a room automatically "glue"
;;; which each other in a visual manner.
;;;
;;; Related systems: render
(s/def ::room boolean?)

;;; Input component
;;;
;;; An entity with this component can receive input of some
;;; type. Dispatch determines further behavior.
;;;
;;; Related systems: input, player
(s/def ::input keyword?)

;;; Mobility component
;;;
;;; Mobile entities have some notion of moving around. Dispatch
;;; determines further behavior.
;;;
;;; Related systems: movement, defer, combat
(s/def ::mobile ::type)

;;; Selector component
;;;
;;; Selectors target other entities in a specific order and can be used to cycle
;;; through them.
;;;
;;; Relatd systems: movement
(s/def ::selector (s/coll-of ::id :into []))

;;; Floor component
;;;
;;; Entities with this component exist only on the current floor denoted.
;;; This is complementary to but not dependent on position.
;;;
;;; Related systems: levels, movement, render, sight, viewport, world
(s/def ::floor integer?)

;;; Foundation component
;;;
;;; This component is specifically made for floors, so each foundation
;;; must also be on a floor on which it also has to be unique.
;;;
;;; Related systems: levels, movement, render, viewport
(s/def ::foundation (s/tuple pos-int? pos-int?))

;;; Color component
;;;
;;; Custom color for rendering.
;;;
;;; Related systems: render
(s/def ::color (s/or :int ::pos-int-or-zero :keyword keyword?))

;;; Renderable component
;;;
;;; A renderable entity an have a visual appearance, further determined
;;; by dispatch and other components.
;;;
;;; Related systems: render, combat
(s/def ::renderable (s/or :type ::type :tile (s/keys :req [::type ::color])))

;;; Messages component
;;;
;;; An entity bearing messages. Used for logs, dialogs, emails and the like.
;;;
;;; Related systems: messages, combat, door, movement, player, render, status
(s/def ::messages (s/coll-of (s/tuple string? pos-int?)))

;;; Deferred action component
;;;
;;; This component is used for temporary entities that exist to fulfill
;;; complex input patterns on behalf of the player entity,
;;; e.g. selection mechanisms.
;;;
;;; Related systems: defer, player, door
(s/def ::deferred ifn?)

;;; AI component
;;;
;;; Entities with this component are being controlled by computer
;;; intelligence and act on their own behalf. Dispatch determines further
;;; behavior.
;;;
;;; Related systems: ai
(s/def ::ai keyword?)

;;; Memory component
;;;
;;; Entities with a memory and remember things, such as the location of
;;; formerly seen entities, also including the layout of levels.
;;;
;;; Related systems: sight, memory, render
(s/def ::memory map?)

;;; Inventory component
;;;
;;; Anything with an inventory can carry stuff around.
;;;
;;; Related systems: inventory
(s/def ::inventory (s/coll-of ::id :into #{}))

;;; Pickup component
;;;
;;; A pickup can be carried around in an `inventory`.
;;; Activation type might require other component(s) present.
;;;
;;; Related systems: inventory
(s/def ::pickup keyword?)

;;; Music component
;;;
;;; Used to trigger the playback of music.
;;;
;;; Related systems: levels, sound
(s/def ::music keyword?)

;;; Slots component
;;;
;;; Enables slotting equipment.
;;;
;;; Related systems:
(s/def ::slots (s/every-kv keyword? (s/nilable ::id)))

;;; Weapon component
;;;
;;; Stats for a weapon.
;;;
;;; Related systems: combat
(s/def ::damage ::pos-int-or-zero)
(s/def ::penetration ::pos-int-or-zero)
(s/def ::transition ::type)
(s/def ::weapon
  (s/keys :req [::type ::damage ::penetration ::transition]))

;;; Compound component
;;;
;;; Compounds glue together entities by reference.
;;;
;;; Related systems: all
(s/def ::parents (s/nilable (s/coll-of ::id :into #{})))
(s/def ::children (s/nilable (s/coll-of ::id :into #{})))
(s/def ::compound (s/keys :opt [::parents ::children]))

;;; Label component
;;;
;;; Used to post-process entities in `levels`.
;;;
;;; Related systems: levels
(s/def ::label keyword?)

;;; Lootable component
;;;
;;; Used for containers. Should usually go together with `inventory`.
;;;
;;; Related systems: loot
(s/def ::lootable boolean?)
