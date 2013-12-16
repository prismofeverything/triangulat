(ns triangulat.util
  (:require 
   [clojure.string :as string]
   [cljs.core.async :refer [chan <! >! put!]]
   [cljs.reader :as reader]
   [domina :as dom]
   [domina.css :as css]
   [domina.events :as events]))

(defn event-chan
  [c id el type data]
  (let [writer #(put! c [id % data])]
    (events/listen! el type writer)
    {:chan c
     :unsubscribe #(.removeEventListener el type writer)}))

(defn key-code
  [event]
  (.-keyCode (events/raw-event event)))

(defn input-value
  [input]
  (-> input css/sel dom/single-node dom/value))

(defn tag
  [type id class]
  (keyword (str (name type) "#" id "." class)))

(defn element-width
  [el]
  (.-width (js/goog.style.getSize (dom/single-node el))))

(defn element-height
  [el]
  (.-height (js/goog.style.getSize (dom/single-node el))))

(defn viewport-to-bottom
  [el]
  (let [inner (dom/single-node el) 
        viewport (.-innerHeight js/window)
        page-top (js/goog.style.getPageOffsetTop inner)
        height (.-height (js/goog.style.getSize inner))]
    (- (+ page-top height) viewport)))

(defn scroll-to-bottom
  [el]
  (let [viewport-top (viewport-to-bottom el)]
    (js/window.scrollTo 0 viewport-top)))

