(ns statehack.entity
  (:require [clojure.set :as set]))

(defn uuid [] (java.util.UUID/randomUUID))

(defn entity [& components]
  (apply merge {:id (uuid)} components))

(defn components [e]
  (set (keys e)))

(defn capable? [e & cs]
  (set/subset? (set cs) (components e)))

(defn filter-capable [[& cs] es]
  (filter #(apply capable? % cs) es))

(defn remove-capable [[& cs] es]
  (remove #(apply capable? % cs) es))

