(ns statehack.component)

(defn unique [type]
  {:unique type})

(defn named [s]
  {:name s})

(defn vulnerable [hp]
  {:pre [(pos? hp)]}
  {:hp {:current hp
        :max hp}})

(defn adaptive [xp lvl]
  {:pre [(>= xp 0) (>= lvl 0)]}
  {:adaptive {:xp xp
              :level lvl}})

(defn skillset [skills]
  {:pre [(map? skills) (every? keyword? (keys skills)) (every? pos? (vals skills))]}
  {:skillset skills})

(defn obstacle []
  {:obstacle true})

(defn door [open?]
  {:open open?})

(defn room []
  {:room true})

(defn input [type]
  {:input type})

(defn mobile [type & opts]
  {:mobile (merge (apply hash-map opts)
                  {:type type})})

(defn position [x y]
  {:pre [(>= x 0) (>= y 0)]}
  {:position [x y]})

(defn renderable [type]
  {:renderable type})

(defn messages [ms]
  {:pre [(coll? ms)]}
  {:messages ms})

(defn deferred [action]
  {:deferred action})

(defn race [type]
  {:race type})

(defn ai [type]
  {:ai {:type type}})
