(defpackage starshot/particle
  (:use :cl)
  (:local-nicknames (:vec :starshot/vector))
  (:local-nicknames (:mop :starshot/mop))
  (:import-from :tactile #:compose #:juxt #:callable #:partialr #:partial)
  (:export #:particle #:make-particle #:calculate-new-position #:calculate-new-velocity #:integrate #:p #:v #:a #:damping #:inverse-mass #:r #:collision? #:calculate-collision))

(in-package :starshot/particle)

(defclass particle ()
  ((p
    :initarg :p
    :accessor p)
   (v
    :initarg :v
    :accessor v)
   (a
    :initarg :a
    :accessor a)
   ;; < 1
   (damping
    :initarg :damping
    :accessor damping)
   ;; Because infinite mass objects are useful
   (inverse-mass
    :initarg :inverse-mass
    :accessor inverse-mass)
   (r
    ;; Radius
    :initarg :r
    :accessor r)
   (tangible
    ;; Boolean
    :initarg :tangible
    :accessor tangible)
   (fixed
    ;; Boolean
    :initarg :fixed
    :accessor fixed
    )))

(defun make-particle (p v a damping inverse-mass r tangible fixed)
  (make-instance 'particle :p p :v v :a a :damping damping :inverse-mass inverse-mass :r r :tangible tangible :fixed fixed))

(defmethod print-object ((p particle) out)
  (print-unreadable-object (p out :type t)
    (format out "POSITION: ~a~%VELOCITY: ~a~%ACCELERATION: ~a~%" (p p) (v p) (a p))))

;; Creating simulations with a g value of 10 m/s2 can look dull and insipid. Most
;; developers use higher values, from around 15 m/s 2 for shooters (to avoid projectiles
;; being accelerated into the ground too quickly) to 20 m/s2 , which is typical of driving
;; games. Some developers go further and incorporate the facility to tune the g value on
;; an object-by-object basis. Our engine will include this facility.

(defmethod calculate-new-position ((part particle) timedelta)
  (vec:vec+ (p part)
            (vec:scalar* timedelta (v part))))

(defmethod calculate-new-velocity ((part particle) timedelta)
  (vec:vec+ (vec:scalar* (expt (damping part) timedelta) (v part))
            (vec:scalar* timedelta (a part))))

(defmethod calculate-new-acceleration ((part particle) (force-acc vec:cartesian))
  (vec:scalar* (inverse-mass part) force-acc))

(defmethod integrate ((part particle) timedelta force-acc)
  (declare (type real timedelta))
  (assert (> timedelta 0.0))
  
  (mop:update-object* part
                      ((:a (calculate-new-acceleration mop::self force-acc))
                       (:v (calculate-new-velocity mop::self timedelta))
                       (:p (calculate-new-position mop::self timedelta)))))

(defmethod collision? ((p1 particle) (p2 particle))
  (let ((difference (vec:magnitude (vec:vec-minus (p p1) (p p2)))))
    (>= (+ (r p1) (r p2)) difference)))

(defmethod momentum ((part particle))
  (/ (v part) (inverse-mass part)))

(defun calculate-collision-in-direction (accessor p1 p2)
  (let* ((u1 (funcall accessor (v p1)))
         (u2 (funcall accessor (v p2)))
         (m1 (/ 1 (inverse-mass p1)))
         (m2 (/ 1 (inverse-mass p2)))
         (v1 (+ (* u1
                   (/ (- m1 m2)
                      (+ m1 m2)))
                (* u2
                   (/ (* 2 m2)
                      (+ m1 m2)))))
         (v2 (+ (* u2
                   (/ (- m2 m1)
                      (+ m1 m2)))
                (* u1
                   (/ (* 2 m1)
                      (+ m1 m2))))))
    (list v1 v2)))

(defmethod calculate-collision ((p1 particle) (p2 particle))
  ;; I have notes on this
  (mapcar
   (lambda (accessor)
     (calculate-collision-in-direction accessor p1 p2))
   (list #'vec:x #'vec:y #'vec:z)))
