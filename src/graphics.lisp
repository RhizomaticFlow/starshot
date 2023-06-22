(defpackage starshot/graphics
  (:use :cl)
  (:local-nicknames (:p :starshot/particle))
  (:local-nicknames (:vec :starshot/vector))
  (:local-nicknames (:p0 :starshot/p0))
  (:import-from :tactile #:partialr)
  (:export :particles))

(in-package :starshot/graphics)

;; INIT SDL
;; SET WINDOWS
;; SET KEYBINDS
;; DRAW STATE
;; UPDATE STATE

(defun particles ()
  (let ((state
          (p0:make-p0
           0.1
           (list
            (p:make-particle (vec:make-cartesian 50 0 0)
                             (vec:make-cartesian 0 10 0)
                             (vec:make-cartesian 0 0 0)
                             1 1/5 10 t nil)
            (p:make-particle (vec:make-cartesian 50 70 0)
                             (vec:make-cartesian 0 0 0)
                             (vec:make-cartesian 0 0 0)
                             1 1/4 8 t nil)
            (p:make-particle (vec:make-cartesian 50 140 0)
                             (vec:make-cartesian 0 0 0)
                             (vec:make-cartesian 0 0 0)
                             1 1/3 6 t nil)
            (p:make-particle (vec:make-cartesian 50 210 0)
                             (vec:make-cartesian 0 0 0)
                             (vec:make-cartesian 0 0 0)
                             1 1/2 4 t nil)
            (p:make-particle (vec:make-cartesian 50 280 0)
                             (vec:make-cartesian 0 0 0)
                             (vec:make-cartesian 0 0 0)
                             1 1 2 t nil)
            (p:make-particle (vec:make-cartesian 200 0 0)
                             (vec:make-cartesian 0 10 0)
                             (vec:make-cartesian 0 0 0)
                             1 1/5 10 t nil)
            (p:make-particle (vec:make-cartesian 200 70 0)
                             (vec:make-cartesian 0 0 0)
                             (vec:make-cartesian 0 0 0)
                             1 1/4 8 t nil)
            (p:make-particle (vec:make-cartesian 200 140 0)
                             (vec:make-cartesian 0 0 0)
                             (vec:make-cartesian 0 0 0)
                             1 1/3 6 t nil)
            (p:make-particle (vec:make-cartesian 200 210 0)
                             (vec:make-cartesian 0 0 0)
                             (vec:make-cartesian 0 0 0)
                             1 1/2 4 t nil)
            (p:make-particle (vec:make-cartesian 200 280 0)
                             (vec:make-cartesian 0 0 0)
                             (vec:make-cartesian 0 0 0)
                             1 1 2 t nil)
            (p:make-particle (vec:make-cartesian 400 0 0)
                             (vec:make-cartesian 0 10 0)
                             (vec:make-cartesian 0 0 0)
                             1 1/5 10 t nil)
            (p:make-particle (vec:make-cartesian 400 70 0)
                             (vec:make-cartesian 0 0 0)
                             (vec:make-cartesian 0 0 0)
                             1 1/4 8 t nil)
            (p:make-particle (vec:make-cartesian 400 140 0)
                             (vec:make-cartesian 0 0 0)
                             (vec:make-cartesian 0 0 0)
                             1 1/3 6 t nil)
            (p:make-particle (vec:make-cartesian 400 210 0)
                             (vec:make-cartesian 0 0 0)
                             (vec:make-cartesian 0 0 0)
                             1 1/2 4 t nil)
            (p:make-particle (vec:make-cartesian 400 280 0)
                             (vec:make-cartesian 0 0 0)
                             (vec:make-cartesian 0 0 0)
                             1 1 2 t nil)
            (p:make-particle (vec:make-cartesian 550 0 0)
                             (vec:make-cartesian 0 10 0)
                             (vec:make-cartesian 0 0 0)
                             1 1/5 10 t nil)
            (p:make-particle (vec:make-cartesian 550 70 0)
                             (vec:make-cartesian 0 0 0)
                             (vec:make-cartesian 0 0 0)
                             1 1/4 8 t nil)
            (p:make-particle (vec:make-cartesian 550 140 0)
                             (vec:make-cartesian 0 0 0)
                             (vec:make-cartesian 0 0 0)
                             1 1/3 6 t nil)
            (p:make-particle (vec:make-cartesian 550 210 0)
                             (vec:make-cartesian 0 0 0)
                             (vec:make-cartesian 0 0 0)
                             1 1/2 4 t nil)
            (p:make-particle (vec:make-cartesian 550 280 0)
                             (vec:make-cartesian 0 0 0)
                             (vec:make-cartesian 0 0 0)
                             1 1 2 t nil)
            )
           600 600 600 :open)))
    
    (sdl:with-init ()
      (sdl:window (p0:width state) (p0:height state) :title-caption "Move a rectangle using the mouse")
      (setf (sdl:frame-rate) 60)
      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event ()
                         (sdl:push-quit-event))
        (:idle ()
               ;; Change the color of the box if the left mouse button is depressed
               ;; Clear the display each game loop
               (sdl:clear-display sdl:*black*)
               ;; Draw the box having a center at the mouse x/y coordinates.
               (loop :for particle :in (p0:particles state)
                     :do
                        (sdl:draw-box (sdl:rectangle-from-midpoint-*
                                       (vec:x (p:p particle))
                                       (vec:y (p:p particle))
                                       (* (p:r particle) 2)
                                       (* (p:r particle) 2))
                                      :color
                                      (sdl:color :r (random 255) :g (random 255) :b (random 255))))
               (p0:iterate-state state)
               ;; Redraw the display
               (sdl:update-display))))))

(defun charges ()
  (let ((state
          (p0:make-p0
           0.1
           (list
            (p:make-particle (vec:make-cartesian 100 100 0)
                             (vec:make-cartesian 40 -40 0)
                             (vec:make-cartesian 0 0 0)
                             1 2 7 t nil -36)
            (p:make-particle (vec:make-cartesian 500 500 0)
                             (vec:make-cartesian -40 40 0)
                             (vec:make-cartesian 0 0 0)
                             1 2 7 t nil 36)
            )
           600 600 600 :open)))
    
    (sdl:with-init ()
      (sdl:window (p0:width state) (p0:height state) :title-caption "Move a rectangle using the mouse")
      (setf (sdl:frame-rate) 60)
      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event ()
                         (sdl:push-quit-event))
        (:idle ()
               ;; Change the color of the box if the left mouse button is depressed
               ;; Clear the display each game loop
               (sdl:clear-display sdl:*black*)
               ;; Draw the box having a center at the mouse x/y coordinates.
               (loop :for particle :in (p0:particles state)
                     :do
                        (sdl:draw-box (sdl:rectangle-from-midpoint-*
                                       (vec:x (p:p particle))
                                       (vec:y (p:p particle))
                                       (* (p:r particle) 2)
                                       (* (p:r particle) 2))
                                      :color
                                      (sdl:color :r (random 255) :g (random 255) :b (random 255)))
                        ;; Draw acceleration vector in green
                        (sdl:draw-line-* (floor (vec:x (p:p particle)))
                                         (floor (vec:y (p:p particle)))
                                         (floor (+ (vec:x (p:p particle)) (vec:x (p:a particle))))
                                         (floor (+ (vec:y (p:p particle)) (vec:y (p:a particle))))
                                         :color
                                         (sdl:color :r 0 :g (+ 50 (random 205)) :b 0))
                        ;; Draw velocity vector in blue
                        (sdl:draw-line-* (floor (vec:x (p:p particle)))
                                         (floor (vec:y (p:p particle)))
                                         (floor (+ (vec:x (p:p particle)) (vec:x (p:v particle))))
                                         (floor (+ (vec:y (p:p particle)) (vec:y (p:v particle))))
                                         :color
                                         (sdl:color :r 0 :g 0 :b (+ 50 (random 205)))))
               (p0:iterate-state state)
               ;; Redraw the display
               (sdl:update-display))))))
