(ns triangulat.svg)

(defn attr 
  [obj attrs]
  (.attr obj (clj->js attrs)))

(defn move-to
  [point]
  (str 
   "M" (js/Math.floor (:x point)) 
   "," (js/Math.floor (:y point))))

(defn line-to
  [point]
  (str 
   "L" (js/Math.floor (:x point)) 
   "," (js/Math.floor (:y point))))

(defn line
  [draw {:keys [begin end color width]}]
  (let [l (.path draw (str (move-to begin) (line-to end)))]
    (attr l {:stroke color :stroke-width width})))

(defn triangle
  [draw {:keys [a b c color border width]}]
  (let [tri (.path draw (str (move-to a) (line-to b) (line-to c) "Z"))]
    (attr tri {:stroke border :stroke-width width :fill color})
    tri))
