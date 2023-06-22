(defpackage starshot/graphics
  (:use :cl)
  (:local-nicknames (:p :starshot/particle))
  (:local-nicknames (:vec :starshot/vector))
  (:local-nicknames (:p0 :starshot/p0))
  (:import-from :tactile #:partialr)
  (:export :particles))

(in-package :starshot/graphics)


(defparameter *random-color* sdl:*white*)

(defun mouse-rect-2d ()
  (sdl:with-init ()
    (sdl:window 600 600 :title-caption "Move a rectangle using the mouse")
    (setf (sdl:frame-rate) 60)

    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event ()
                       (sdl:push-quit-event))
      (:idle ()
             ;; Change the color of the box if the left mouse button is depressed
             (setf *random-color* (sdl:color :r (random 255) :g (random 255) :b (random 255)))

             ;; Clear the display each game loop
             (sdl:clear-display sdl:*black*)

             ;; Draw the box having a center at the mouse x/y coordinates.
             (sdl:draw-box (sdl:rectangle-from-midpoint-* (sdl:mouse-x) (sdl:mouse-y) 20 20)
                           :color *random-color*)

             ;; Redraw the display
             (sdl:update-display)))))

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
