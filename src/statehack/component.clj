(ns statehack.component
  "All statehack components")

(defn unique
  "Unique entity component

  Every `type` must only exist at most once among all entities.

  Related systems: unique"
  [type]
  {:unique type})

(defn name
  "Name component

  Gives an entity the name `s`.

  Related systems: name, status"
  [s]
  {:name s})

(defn position
  "Position component

  Entities with this component exist on a linear plane at the current
  coordinates. This does not imply they are visible or can't coexist
  at the same location.

  Related systems: movement, levels, defer, render, sight, viewport, world"
  [[x y]]
  {:pre [(>= x 0) (>= y 0)]}
  {:position [x y]})

(defn category
  "Category component

  Simply used to further specify the `type` of an entity to influence
  rendering and, possibly, other systems.

  Related systems: name"
  [type]
  {:category type})

(defn vulnerable
  "Vulnerability component

  A vulnerable entity can be damaged and has a notion of health.

  Related systems: combat, status"
  [hp]
  {:pre [(pos? hp)]}
  {:hp {:current hp
        :max hp}})

(defn alive
  "Life component

  Determines whether an entity can be any of the two states of alive
  and dead.

  Related systems: combat"
  [alive?]
  {:alive alive?})

(defn adaptive
  "Adaptivity component

  An adaptive component learns by accomplishing things and can improve
  upon its experience.

  Related systems: status"
  [xp lvl]
  {:pre [(>= xp 0) (>= lvl 0)]}
  {:adaptive {:xp xp
              :level lvl}})

(defn skillset
  "Skillset component

  A skilled entity can perform actions other than movement.

  Related systems: combat"
  [skills]
  {:pre [(map? skills) (every? keyword? (keys skills)) (every? pos? (vals skills))]}
  {:skillset skills})

(defn obstacle
  "Obstacle component

  Entities that are also obstacles can't co-exist at the same
  position, i.e. they obstruct each other. Typical obstacles include
  walls, closed doors, force fields, enemies, the player.  This
  component gets dynamically dispatched, so `type` can take other
  values than `true` and `false`.

  Related systems: obstacle"
  ([type] {:obstacle type})
  ([] (obstacle true)))

(defn door
  "Door component

  A door can be open or closed and behave differently depending on that.

  Related systems: door, obstacle, levels, render"
  [open?]
  {:open open?})

(defn room
  "Room component

  Entities which are considered part of a room automatically \"glue\"
  which each other in a visual manner.

  Related systems: render"
  []
  {:room true})

(defn input
  "Input component

  An entity with this component can receive input of some
  `type`. Dispatch determines further behavior.

  Related systems: input, player"
  [type]
  {:input type})

(defn mobile
  "Mobility component

  Mobile entities have some notion of moving around. Dispatch
  determines further behavior.

  Related systems: movement, defer, combat"
  [type & opts]
  {:mobile (merge (apply hash-map opts)
                  {:type type})})

(defn floor
  "Floor component

  Entities with this component exist only on the current floor denoted
  by `n`. This is complementary to but not dependent on `position`.

  Related systems: levels, movement, render, sight, viewport, world"
  [n]
  {:pre [(integer? n)]}
  {:floor n})

(defn foundation
  "Foundation component

  This component is specifically made for floors, so each foundation
  must also be \"on\" a floor on which it also has to be unique.
  `[w h]` denotes the proportions of the floor.

  Related systems: levels, movement, render, viewport"
  [[w h]]
  {:pre [(>= w 0) (>= h 0)]}
  {:foundation [w h]})

(defn renderable
  "Renderable component

  A renderable entity an have a visual appearance, further determined
  by dispatch against `type` and other components.

  Related systems: render, combat"
  [type]
  {:renderable type})

(defn messages
  "Message component

  An entity bearing messages. Used for logs, dialogs, emails and the like.

  Related systems: messages, combat, door, movement, player, render, status"
  [& ms]
  {:pre [(every? string? ms)]}
  {:messages ms})

(defn deferred
  "Deferred action component

  This component is used for temporary entities that exist to fulfill
  complex input patterns on behalf of the player entity,
  e.g. selection mechanisms.

  Related systems: defer, player, door"
  [action]
  {:deferred action})

(defn ai
  "AI component

  Entities with this component are being controlled by computer
  intelligence and act on their own behalf. Dispatch on `type`
  determines further behavior.

  Related systems: ai"
  [type]
  {:ai {:type type}})

(defn sight
  "Sight component

  Seeing entities can detect the presence or absence of other entities
  within `distance`, as dispatch on `type` determines.

  Related systems: sight, memory, render"
  [type distance]
  {:sight {:type type
           :distance distance}})

(defn memory
  "Memory component

  Entities with a memory and remember things, such as the location of
  formerly seen entities, also including the layout of levels.

  Related systems: sight, memory, render"
  []
  {:memory {:map []}})
