;;;; This file is part of statehack.
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

(ns statehack.system.layout
  "Layouting facility"
  (:require [halo.graphics :as graphics]
            [clojure.zip :as zip]
            [clojure.walk :as walk]
            [statehack.entity :as entity]
            [statehack.system.inventory :as inventory]
            [statehack.system.input.receivers :as receivers]
            [statehack.util :as util]))

(def render-hierarchy "Hierarchy for `render`" (make-hierarchy))

(defn derive-render
  "Derive for `render-hierarchy`"
  [tag parent]
  (alter-var-root #'render-hierarchy derive tag parent))

(defn render-dispatch
  "Dispatch for `render`"
  [e [w h]]
  (e :type))

(defmulti render
  "Render layout element `e`"
  {:arglists '([e [w h]])}
  #'render-dispatch :hierarchy #'render-hierarchy)

(defmethod render :view [e [w h]]
  (assoc e :dimensions [w h]))

(defmethod render :box [{:keys [alignment children] :as e} [w h]]
  (let [[primary secondary] (if (= alignment :horizontal) [w h] [h w])
        fixed-size (apply + (map (fn [{:keys [size]}] (or size 0)) children))
        rest (max (- primary fixed-size) 0)]
    (assoc e
           :children (map (fn [{:keys [size] :as e}]
                            (let [size (or size rest)]
                              (render e (if (= alignment :horizontal) [size h] [w size]))))
                          children)
           :dimensions [w h])))

(defmethod render :stack [{:keys [children] :as e} [w h]]
  (assoc e
         :children (map #(render % [w h]) children)
         :dimensions [w h]))

(def render-mem (memoize render))

;;; Layout Elements

(defn element [type opts]
  (merge {:visible true}
         (select-keys opts [:visible :size :id])
         {:type type}))

(defn container [type opts & children]
  (merge (element type opts)
         {:children children}))

(defn box [opts & children]
  (merge {:alignment :horizontal}
         (apply container :box opts children)
         (select-keys opts [:alignment])))

(def stack (partial container :stack))

(defn view
  ([binding opts]
   (merge (element :view opts)
          {:binding binding}))
  ([binding] (view binding {})))

(def layout
  (stack {}
   (box {:alignment :horizontal :visible inventory/inventory-open?}
        (view :inventory)
        (view :floor {:size 40}))
   (box {:alignment :vertical}
        (view :status {:size 1})
        (view :world {:id :world-view})
        (view :messages {:size 5}))))

(defn layout-zipper [layout]
  (zip/zipper (constantly true) :children (fn [node xs] (assoc node :children xs)) layout))

(defn eval-bindings [game element]
  (into {} (for [[k v] element]
             [k (if (fn? v) (v game) v)])))

(defn eval-layout-bindings [game layout]
  (walk/postwalk #(if (map? %) (eval-bindings game %) %) layout))

(defn by-id [layout]
  (util/index-by :id (filter :id (map zip/node (take-while (complement zip/end?) (iterate zip/next (layout-zipper layout)))))))

(defn system
  "Determine the layout dimensions"
  [{:keys [graphics] :as game}]
  (assoc game :layout (eval-layout-bindings game (render-mem layout (graphics/size graphics)))))
