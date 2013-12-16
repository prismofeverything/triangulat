(ns triangulat.core
  (:require 
   [clojure.string :as string]
   [cljs.core.async :refer [chan <! >! put!]]
   [cljs.reader :as reader]
   [domina :as dom]
   [domina.css :as css]
   [domina.events :as events]
   [singult.core :as sing]
   [triangulat.connect :as connect]
   [triangulat.util :as util]
   [triangulat.svg :as svg])
  (:require-macros 
   [cljs.core.async.macros :refer [go]]))

(def send (chan))
(def receive (chan))

(def ws-url "ws://localhost:19991/async")
(def ws (new js/WebSocket ws-url))
(def keyboard (atom {}))

(def board (atom nil))

(def sqrt3 (Math/sqrt 3.0))
(def paper-dimensions {:width 1000 :height 1000})
(def draw (js/Snap (:width paper-dimensions) (:height paper-dimensions)))
(def canonical-triangle 
  {:color "#999999"
   :border "#ffffff"
   :width 0})

(def board-center 
  {:x (* 0.5 (:width paper-dimensions))
   :y (* 0.5 (:height paper-dimensions))})

(defn log
  [e]
  (.log js/console e))

(def triangle-colors
  {:a "#88bb33"
   :b "#8833bb"
   :c "#000000"
   :off (:color canonical-triangle)})

(def color-cycle 
  [(:a triangle-colors) (:b triangle-colors) (:c triangle-colors)])

(def color-map
  {:a 0 :b 1 :c 2})

(defn rotate
  [l]
  (cons (last l) (take 2 l)))

(defn triangle-symmetry
  [name]
  (take 3 (iterate rotate name)))

(defn colorize
  [triangle color]
  (let [symmetry (triangle-symmetry (:name triangle))
        start (get color-map color)
        indexes (map #(mod % 3) (range start (+ start 3)))]
    (doall
     (map
      (fn [sym index]
        (let [tri (get @board sym)]
          (svg/attr (:svg tri) {:fill (nth color-cycle index)})))
      symmetry indexes))))

(defn create-triangle
  [points a b c]
  (let [svg (svg/triangle draw (merge canonical-triangle points))
        name (list a b c)
        triangle {:svg svg :name name :color (atom :off) :tentative (atom nil)}]
    (.click 
     (:svg triangle) 
     (fn [p] 
       (log (str name))
       (cond
        (get @keyboard 65) (colorize triangle :a)
        (get @keyboard 66) (colorize triangle :b)
        (get @keyboard 67) (colorize triangle :c)
        :else (colorize triangle :off))))
    triangle))

(defn find-down-points
  [top-left length]
  {:a top-left 
   :b (update-in top-left [:x] (partial + length))
   :c {:x (+ (:x top-left) (* 0.5 length))
       :y (+ (:y top-left) (* 0.5 sqrt3 length))}})

(defn find-up-points
  [top length]
  (let [x (:x top)
        y (+ (:y top) (* 0.5 sqrt3 length))]
    {:a top
     :b {:x (- x (* 0.5 length)) :y y}
     :c {:x (+ x (* 0.5 length)) :y y}}))

(defn create-triangular-lattice
  [top-left divisions length find-points]
  (let [half-division (* length 0.5)]
    (doall 
     (mapcat
      (fn [layer] 
        (let [layer-divisions (- divisions layer)
              tl {:x (+ (:x top-left) (* layer half-division))
                  :y (+ (:y top-left) (* layer half-division sqrt3))}]
          (map
           (fn [division]
             (create-triangle
              (find-points 
               (update-in 
                tl [:x] 
                #(+ % (* division length))) length)
              (inc layer)
              (- layer-divisions division)
              (inc division)))
           (range layer-divisions))))
      (range divisions)))))

(defn draw-triangular-lattice
  [top-left length divisions]
  (let [division-length (/ length divisions)

        downs (create-triangular-lattice 
               top-left 
               divisions 
               division-length 
               find-down-points)

        ups (create-triangular-lattice 
             (update-in top-left [:x] (partial + division-length)) 
             (dec divisions) 
             division-length 
             find-up-points)]

    (reduce
     (fn [lattice triangle]
       (assoc lattice (:name triangle) triangle))
     {} (concat downs ups))))

(defn init
  [data]
  (let [lattice (draw-triangular-lattice {:x 20 :y 20} 800 15)]
    (reset! board lattice)
    (log (str (keys lattice)))))

(defn dispatch-message
  []
  (go
   (while true
     (let [msg (<! receive)
           raw (.-data msg)
           data (reader/read-string raw)]
       (condp = (:op data)
         :init (init data)
         (log (str "op not supported! " data)))))))

(defn key-code
  [event]
  (.-keyCode (events/raw-event event)))

(defn key-down
  [k]
  (swap! keyboard #(assoc % (key-code k) true)))

(defn key-up
  [k]
  (swap! keyboard #(dissoc % (key-code k))))

(defn make-sender
  []
  (log "HELLO")
  (util/event-chan send :keydown js/document.body :keydown {})
  (util/event-chan send :keyup js/document.body :keyup {})
  ;; (util/event-chan send :click js/document.body :click {})
  (go
   (while true
     (let [[id event data] (<! send)]
       (condp = id
         :keydown (key-down event)
         :keyup (key-up event)
         :click (log "click!"))))))

(defn make-receiver []
  (set! 
   (.-onmessage ws)
   (fn [msg]
     (put! receive msg)))
  (set!
   (.-onopen ws)
   (fn [msg] 
     (.send ws {:op :init})))
  (dispatch-message))

(defn init!
  []
  (make-sender)
  (make-receiver))

(def on-load
  (set! (.-onload js/window) init!))

(connect/connect)

