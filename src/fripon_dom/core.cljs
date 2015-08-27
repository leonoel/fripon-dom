(ns fripon-dom.core
  (:require-macros [fripon.ecs :refer [defentity entity
                                       defcomponent component just this parent system]]
                   [fripon.frp :refer [in-context-sync]])
  (:require [fripon.frp :as frp]
            [fripon.ecs :refer [cmap cpmap spawn-entity]]
            [goog.dom]
            [goog.style]
            [goog.events]))

(def context (frp/make-context))

(defn from-event
  ([target type]
   (from-event target type (frp/make-stream)))
  ([target type dispose]
   (frp/from-callback-sync
    (fn [cb]
      (goog.events/listen target type cb)
      (frp/watch (fn [_] (goog.events/unlisten target type cb)) dispose)))))

(defn load-image [src]
  (let [image (js/Image.)]
    (set! (.-src image) src)
    image))

(defn current-viewport-size []
  (let [size (.getViewportSize goog.dom)]
    {:width (.-width size)
     :height (.-height size)}))

(defentity positioned
  [_ (cmap (fn [element x y]
             (frp/watch #(goog.style/setStyle element "left" (str % "px")) x)
             (frp/watch #(goog.style/setStyle element "top" (str % "px")) y))
           (this element) (this x) (this y))])

(defentity fixed
  positioned
  [_ (cmap (fn [element]
             (goog.style/setStyle element "position" "fixed"))
           (this element))])

(defentity absolute
  positioned
  [_ (cmap (fn [element]
             (goog.style/setStyle element "position" "absolute"))
           (this element))])

(defentity fullscreen fixed
  [viewport-size (cmap (fn [dispose]
                         (frp/hold
                          (current-viewport-size)
                          (frp/map (fn [_] (current-viewport-size))
                                   (from-event js/window "resize" dispose)))) (this dispose))
   x      (just (frp/forever 0))
   y      (just (frp/forever 0))
   width  (cpmap :width (this viewport-size))
   height (cpmap :height (this viewport-size))])

(def raf (.-requestAnimationFrame js/window))

(defentity animated
  [animate (cmap (fn [dispose]
                   (let [stopped (frp/hold false dispose)]
                     (frp/from-callback-sync
                      (fn [cb]
                        ((fn a [] (when-not @stopped
                                    (cb :animate)
                                    (raf a))))))))
                 (this dispose))
   _ (cmap (fn [animate repaint-fns]
             (frp/watch (fn [_] (doseq [f @repaint-fns] (f))) animate))
           (this animate) (system repaint))])

(defn dom-element [type]
  (entity
   [element (cmap (fn [children]
                    (apply goog.dom/createDom type (js-obj) @children))
                  (system element))]))

(defentity div
  (dom-element "div")
  [_ (cmap (fn [element width height]
             (frp/watch #(goog.style/setStyle element "width" (str % "px")) width)
             (frp/watch #(goog.style/setStyle element "height" (str % "px")) height))
           (this element) (this width) (this height))])

(defentity canvas2d
  (dom-element "canvas")
  [_ (cmap (fn [element width height]
             (frp/watch #(.setAttribute element "width" %) width)
             (frp/watch #(.setAttribute element "height" %) height))
           (this element) (this width) (this height))])

(defn z-index [i]
  (entity
   [_ (cmap (fn [element] (goog.style/setStyle element "z-index" i)) (this element))]))

(defn mount! [element-entity parent]
  (let [dispose (in-context-sync
                 context
                 (spawn-entity
                  (entity
                   element-entity
                   [_ (cmap (fn [element dispose]
                              (goog.dom/appendChild parent element)
                              (frp/watch
                               (fn [_] (goog.dom/removeNode element)) dispose))
                            (this element) (this dispose))])))]
    (fn [] (frp/emit-sync! dispose :dispose))))
