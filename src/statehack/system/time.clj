(ns statehack.system.time)

(defn pass-time [game]
  (assoc game :time true))
