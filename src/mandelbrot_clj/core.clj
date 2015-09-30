(ns mandelbrot-clj.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [complex.core :as c]))


(defn scale-pixel-to-complex [[x y] center width window-dim]
  (c/complex
   (+ (c/real-part center) (* width (/ (- x (/ window-dim 2)) window-dim)))
   (+ (c/imaginary-part center) (* width (/ (- y (/ window-dim 2)) window-dim)))))

;; the below functions compute membership in the Mandelbrot set using
;; lazy seqs. while this is "good" clojure, they are also much slower
;; than a less-idiomatic loop/recur
;; (defn mandelbrot-seq
;;   "A lazy sequence of Mandelbrot iterations for a given c."
;;   ([c] (mandelbrot-seq (c/complex 0 0) c))
;;   ([z c] (cons z (lazy-seq (mandelbrot-seq (c/+ (c/* z z) c) c)))))

;; (defn mandel-iter
;;   "A lazy sequence indexed by iteration number"
;;   [c]
;;   (map-indexed (fn [a b] [a b]) (mandelbrot-seq c)))

;; (defn escape-number [c maxiter]
;;   (letfn [(escaped? [[ind z]]
;;             (or (> ind maxiter)
;;                 (or (< 2 (c/real-part z))
;;                     (< 2 (c/imaginary-part z)))))]
;;     (first (filter escaped? (mandel-iter c)))))

(defn escape-to-color [en maxiter]
  (let [rat (/ (en 0) maxiter)]
    (q/color (* 100 rat) 100 (* 100  (- 1 rat)))))

(defn escape-number-loop [c maxiter]
  (loop [z (c/complex 0 0)
         iter 0]
    (if (or (> iter maxiter)
            (or (> (c/real-part z) 2)
                (> (c/imaginary-part z) 2)))
      [iter z]
      (recur (c/+ (c/* z z) c) (inc iter)))))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  (q/color-mode :hsb 100)
  {:center (c/complex -0.6 0.6)
   :width 1.0})

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  state)

(defn draw-state [state]
  (let [w (q/width)
        h (q/height)
        pxl (q/pixels)]
    (doseq [x (range w)
            y (range h)]
      (let [c (scale-pixel-to-complex [x y] (:center state) (:width state) w)
            mycolor (escape-to-color (escape-number-loop c 100) 100)]
        (aset-int pxl (+ x (* y w)) mycolor)))
    (q/update-pixels))
)

(q/defsketch mandelbrot-clj
 :title "FRACTALS"
 :size [500 500]
 :setup setup
 :update update-state
 :draw draw-state
 :renderer :p2d
 :features [:keep-on-top]
 :middleware [m/fun-mode])
