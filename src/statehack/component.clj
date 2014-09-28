(ns statehack.component)

(defn door [open?]
  {:open open?})

(defn room []
  {:room true})

(defn input [type]
  {:input type})

(defn mobile [type]
  {:mobile type})

(defn position [x y]
  {:pre [(>= x 0) (>= y 0)]}
  {:position [x y]})

(defn renderable [type]
  {:renderable type})

(defn messages [ms]
  {:pre [(coll? ms)]}
  {:messages ms})
