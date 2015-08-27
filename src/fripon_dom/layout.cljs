(ns fripon-dom.layout
  (:require-macros
   [fripon.ecs :refer [defentity defcomponent entity just this parent system child]])
  (:require
   [fripon-dom.core :as dom]
   [fripon-dom.math :refer [half half-diff]]
   [fripon.frp :as frp]
   [fripon.ecs :refer [cmap cpmap down merge-entities]]))

(def conversions ;; cumbersome... maybe clever solution using core.logic ?
  {[:low :high :length] #(- %2 %1)
   [:low :middle :high] #(- (* %1 2) %2)
   [:low :middle :length] #(- %1 (/ %2 2))

   [:high :low :length] +
   [:high :low :middle] #(- (* %2 2) %1)
   [:high :middle :length] #(+ %1 (/ %2 2))

   [:middle :low :high] #(/ (+ %2 %1) 2)
   [:middle :low :length] #(+ %1 (/ %2 2))
   [:middle :high :length] #(- %1 (/ %2 2))

   [:length :low :high] #(- %2 %1)
   [:length :low :middle] #(* (- %2 %1) 2)
   [:length :middle :high] #(* (- %2 %1) 2)})

(def all [:low :middle :high :length])

(defn expand [values]
  (let [defined (remove (comp nil? values) all)
        count-defined (count defined)]
    (println "defined" defined)
    (cond (< count-defined 2) (throw (ex-info "Too few constraints" {}))
          (> count-defined 2) (throw (ex-info "Too much constraints" {})))
    [(or (:low values) (apply frp/pmap
                              (conversions (into [:low] defined))
                              (map values defined)))
     (or (:length values) (apply frp/pmap
                                 (conversions (into [:length] defined))
                                 (map values defined)))]))

(defn resolve-constraints [{:keys [x left h-center right width
                                   y top v-center bottom height]}]
  (let [[x width] (expand {:low (or x left)
                           :middle h-center
                           :high right
                           :length width})

        [y height] (expand {:low (or y top)
                            :middle v-center
                            :high bottom
                            :length height})]
    (frp/pmap (fn [x y width height]
                {:x      x
                 :y      y
                 :width  width
                 :height height}) x y width height)))

(defentity guides
  [left      (just (frp/forever 0))
   h-center  (cpmap half (this width))
   right     (this width)

   top       (just (frp/forever 0))
   v-center  (cpmap half (this height))
   bottom    (this height)])

(defentity constrained
  [layout (cmap (fn [constraints]
                  (frp/switch
                   (frp/pmap
                    (fn [constraints]
                      (resolve-constraints (into {} constraints))) constraints)))
                (system layout-constraint))

   x         (cpmap :x (this layout))
   y         (cpmap :y (this layout))
   width     (cpmap :width (this layout))
   height    (cpmap :height (this layout))])

(defn constraints [& cs]
  (apply
   merge-entities
   (map (fn [[type comp]]
          (child [layout-constraint (cmap (partial vector type) (down comp))]))
        (partition 2 cs))))

(defentity container dom/div dom/absolute constrained guides)
(defentity fullscreen-container dom/div dom/fixed dom/fullscreen guides)

(defn fit-to-parent-preserve-ratio [w h]
  (let [target-ratio (/ w h)]
    (entity dom/div dom/absolute guides
            [scale (cpmap (fn [pw ph]
                            (if (< (/ pw ph) target-ratio)
                              (/ pw w)
                              (/ ph h))) (parent width) (parent height))
             width (cpmap (partial * w) (this scale))
             height (cpmap (partial * h) (this scale))
             x (cpmap half-diff (parent width) (this width))
             y (cpmap half-diff (parent height) (this height))])))

(def fill-parent
  (constraints
   :left (parent left)
   :right (parent right)
   :top (parent top)
   :bottom (parent bottom)))

