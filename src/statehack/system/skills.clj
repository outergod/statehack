(ns statehack.system.skills)

(defn entity-skill [e name]
  (get-in e [:skillset name]))

(defn type-skills [e type]
  (let [ss (:skillset e)]
    (filter #(= (get-in ss [% :type]) type)
            (keys ss))))

(defn any-type-skill [e type]
  (first (type-skills e type)))
