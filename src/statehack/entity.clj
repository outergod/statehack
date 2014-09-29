(ns statehack.entity
  (:require [clojure.set :as set]))

(defn uuid [] (java.util.UUID/randomUUID))

(defn entity [& components]
  (apply merge {:id (uuid)} components))

(defn capable? [e & cs]
  (set/subset? (set cs) (set (keys e))))

(defn filter-capable [[& cs] es]
  (filter #(apply capable? % cs) es))
