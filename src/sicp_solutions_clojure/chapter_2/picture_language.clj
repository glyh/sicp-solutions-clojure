(ns sicp-solutions-clojure.chapter-2.picture-language
  (:require 
   [quil.core :as q]
   [quil.middleware :as m]))

(def make-vect vector)
(def xcor-vect first)
(def ycor-vect second)

(defn add-vect [v1 v2] 
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) 
             (+ (ycor-vect v1) (ycor-vect v2))))
(defn sub-vect [v1 v2] 
  (make-vect (- (xcor-vect v1) (xcor-vect v2)) 
             (- (ycor-vect v1) (ycor-vect v2))))
(defn scale-vect [s v] 
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

(def make-segment vector)
(def start-segment first)
(def end-segment second)

(defn make-frame [origin edge1 edge2] (vector origin edge1 edge2))

(def origin-frame first)
(defn edge1-frame [frame] (second frame)) 
(defn edge2-frame [frame] (last frame))

(defn frame-coord-map [frame] 
  (fn [v] 
    (add-vect (origin-frame frame) 
    (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
              (scale-vect (ycor-vect v) (edge2-frame frame))))))

(defn draw-line [begin end] (q/line begin end))
(defn segments->painter [segments]
  (fn [frame] 
    (doseq [segment segments]
      (draw-line ((frame-coord-map frame) (start-segment segment))
                 ((frame-coord-map frame) (end-segment segment))))))

(defn transform-painter [painter origin corner1 corner2]
  (fn [frame]
    (let [m          (frame-coord-map frame)
          new-origin (m origin)]
      (painter (make-frame new-origin 
                           (sub-vect (m corner1) new-origin)
                           (sub-vect (m corner2) new-origin))))))

(defn flip-vert [painter]
  (transform-painter painter (make-vect 0 1) (make-vect 1 1) (make-vect 0 0)))

(defn flip-horiz [painter]
  (transform-painter painter (make-vect 1 0) (make-vect 0 0) (make-vect 1 1)))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color 0
   :angle 0})

(defn transform-painter [painter origin corner1 corner2]
  (fn [frame]
    (let [m          (frame-coord-map frame)
          new-origin (m origin)]
      (painter (make-frame new-origin 
                           (sub-vect (m corner1) new-origin)
                           (sub-vect (m corner2) new-origin))))))

(defn transform-painter [painter origin corner1 corner2]
  (fn [frame]
    (let [m          (frame-coord-map frame) 
          new-origin (m origin)]
      (painter (make-frame new-origin 
                           (sub-vect (m corner1) new-origin)
                           (sub-vect (m corner2) new-origin))))))

(defn flip-vert [painter]
  (transform-painter painter (make-vect 0 1) (make-vect 1 1) (make-vect 0 0)))

(defn flip-horiz [painter]
  (transform-painter painter (make-vect 1 0) (make-vect 0 0) (make-vect 1 1)))

(defn rotate90 [painter]
  (transform-painter painter (make-vect 1 0) (make-vect 1 1) (make-vect 0 0)))

(defn rotate180 [painter]
  (transform-painter painter (make-vect 1 1) (make-vect 0 1) (make-vect 1 0)))

(defn rotate270 [painter]
  (transform-painter painter (make-vect 0 1) (make-vect 0 0) (make-vect 1 1)))

 (defn beside 
  ([painter] (beside painter painter))
  ([painter1 painter2]
  (let [split-point (make-vect 1/2 0)
        paint-left  (transform-painter painter1 
                                       (make-vect 0 0)
                                       split-point
                                       (make-vect 0 1))
        paint-right (transform-painter painter2 
                                       split-point 
                                       (make-vect 1 0) 
                                       (make-vect 1/2 1))]
    (fn [frame] (paint-left frame) (paint-right frame)))))

(defn below 
  ([painter] (below painter painter))
  ([painter1 painter2]
  (let [split-point (make-vect 0 1/2)
        paint-up    (transform-painter painter1 
                                       split-point 
                                       (make-vect 1 1/2) 
                                       (make-vect 0 1))
        paint-down  (transform-painter painter2 
                                       (make-vect 0 0) 
                                       (make-vect 1 0) 
                                       split-point)]
    (fn [frame] (paint-up frame) (paint-down frame)))))

(defn below2 [painter1 painter2]
  (rotate270 (beside (rotate90 painter1) (rotate90 painter2))))

(defn above
  ([painter] (above painter painter))
  ([painter1 painter2] (below painter2 painter1)))

(defn split [painter n op1 op2] 
  (if (= n 0) painter 
    (let [smaller (split painter (- n 1) op1 op2)]
      (op1 painter (op2 smaller smaller)))))

(def right-split #(split %1 %2 beside below))
(def up-split #(split %1 %2 above beside))

(defn corner-split [painter n]
  (if (= n 0) painter 
    (let [n- (- n 1)]
      (below (beside (beside (up-split painter n-))
                     (corner-split painter n-))
             (beside painter
                     (below (right-split painter n-)))))))

(defn corner-split-single [painter n]
  (if (= n 0) painter 
    (let [n- (- n 1)]
      (below (beside (up-split painter n-)
                     (corner-split painter n-))
             (beside painter
                     (right-split painter n-))))))

(defn square-limit [painter n]
  (let [quarter (corner-split painter n)
        half    (beside (flip-horiz quarter) quarter)]
    (above (flip-vert half) half)))

(defn shape [point-list]
  (let [cnt (count point-list)]
    (map #(make-segment (nth point-list %)
                        (nth point-list (mod (+ % 1) cnt)))
         (range cnt))))

(defn comp-painter [painters]
  (fn [frame] (doseq [p painters] (p frame))))

(def outline (shape (list (make-vect 0 0)
                          (make-vect 0 1)
                          (make-vect 1 1)
                          (make-vect 1 0))))
(def X (list (make-segment (make-vect 0 0) (make-vect 1 1)) 
             (make-segment (make-vect 0 1) (make-vect 1 0))))
(def diamond (shape (list (make-vect 0 1/2)
                          (make-vect 1/2 1) 
                          (make-vect 1 1/2) 
                          (make-vect 1/2 0))))
(def wave (shape (map #(apply make-vect %) '(
  (1/3 1)       (2/3 1)      (11/14 5/6)   (2/3 2/3)     (2/3 7/12)    
  (11/14 7/12)  (1 5/12)     (1 1/3)       (2/3 1/2)     (2/3 1/3)
  (11/14 0)     (2/3 0)      (1/2 1/4)     (1/3 0)       (1/3 5/12) 
  (3/28 1/3)    (0 2/3)      (0 5/6)       (3/28 7/12)   (1/3 2/3)
  (3/14 5/6)))))
(def smile (shape (list (make-vect 5/12 5/6) 
                        (make-vect 1/2 3/4) 
                        (make-vect 7/12 5/6))))
(def figures {:outline outline, 
              :X       X, 
              :diamond diamond,
              :wave    wave, 
              :smile   smile})
(def painter-figures (into {} (map #(vector (first %) 
                                            (segments->painter (second %))) 
                                   figures)))
           
(defn setup []
  (q/frame-rate 1)
  (q/color-mode :rgb))

(def standard-frame (make-frame (make-vect 0 500) 
                                (make-vect 500 0)
                                (make-vect 0 -500)))

(defn show-sketch [] 
  (comment
    (q/sketch 
      :title "SICP Image Abstraction Corner Single"
      :size [500 500]
      :setup setup
      :middleware [m/fun-mode]
      :draw (fn [state]
        (q/clear)
        (q/background 255)
        (q/stroke-weight 3)
        (q/stroke 0)
        ((corner-split-single (:wave painter-figures) 4) standard-frame)))

    (q/sketch 
      :title "SICP Image Abstraction Smiling Wave"
      :size [500 500]
      :setup setup
      :middleware [m/fun-mode]
      :draw (fn [state]
        (q/clear)
        (q/background 255)
        (q/stroke-weight 3)
        (q/stroke 0)
        ((comp-painter (list (:wave painter-figures) 
                             (:smile painter-figures))) 
         standard-frame))))
  (q/sketch 
    :title "SICP Image Abstraction Square Limit"
    :size [500 500]
    :setup setup
    :middleware [m/fun-mode]
    :draw (fn [state]
      (q/clear)
      (q/background 255)
      (q/stroke-weight 3)
      (q/stroke 0)
      ((square-limit (:wave painter-figures) 3) standard-frame))))
