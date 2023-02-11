(defpackage starshot/particle
  (:use :cl)
  (:local-nicknames (:vec :starshot/vector))
  (:local-nicknames (:mop :starshot/mop))
  (:import-from :starshot/combinators #:compose #:juxt #:callable #:partialr #:partial)
  (:export #:particle #:make-particle #:calculate-new-position #:calculate-new-velocity #:integrate #:p #:v #:a #:damping #:inverse-mass))

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
    :accessor inverse-mass)))

(defmethod make-particle ((p vec:cartesian) (v vec:cartesian) (a vec:cartesian) damping inverse-mass)
  (make-instance 'particle :p p :v v :a a :damping damping :inverse-mass inverse-mass))

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
  (vec:vec+ (a part) force-acc))

(defmethod integrate ((part particle) timedelta force-acc)
  (declare (type real timedelta))
  (assert (> timedelta 0.0))
  (mop:update-object* part
                          ((:p (calculate-new-position mop::self timedelta))
                           (:a (calculate-new-acceleration mop::self force-acc))
                           (:v (calculate-new-velocity mop::self timedelta)))))
