(ns statehack.component)

(defn unique [type]
  {:unique type})

(defn named [s]
  {:name s})

(defn vulnerable [hp]
  {:pre [(pos? hp)]}
  {:hp {:current hp
        :max hp}})

(defn alive [alive?]
  {:alive alive?})

(defn adaptive [xp lvl]
  {:pre [(>= xp 0) (>= lvl 0)]}
  {:adaptive {:xp xp
              :level lvl}})

(defn skillset [skills]
  {:pre [(map? skills) (every? keyword? (keys skills)) (every? pos? (vals skills))]}
  {:skillset skills})

(defn obstacle
  ([type] {:obstacle type})
  ([] (obstacle true)))

(defn door [open?]
  {:open open?})

(defn room []
  {:room true})

(defn input [type]
  {:input type})

(defn mobile [type & opts]
  {:mobile (merge (apply hash-map opts)
                  {:type type})})

(defn position [[x y]]
  {:pre [(>= x 0) (>= y 0)]}
  {:position [x y]})

(defn floor [n]
  {:pre [(integer? n)]}
  {:floor n})

(defn foundation [[w h]]
  {:pre [(>= w 0) (>= h 0)]}
  {:foundation [w h]})

(defn renderable [type]
  {:renderable type})

(defn messages [& ms]
  {:pre [(every? string? ms)]}
  {:messages ms})

(defn deferred [action]
  {:deferred action})

(defn race [type]
  {:race type})

(defn ai [type]
  {:ai {:type type}})

(defn sight [type distance]
  {:sight {:type type
           :distance distance}})

(defn memory []
  {:memory {:map []}})
