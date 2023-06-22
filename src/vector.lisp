(defpackage starshot/vector
  (:use :cl)
  (:import-from :tactile #:compose #:juxt #:callable #:partialr #:partial #:reduce-apply)
  (:export #:make-cartesian #:magnitude #:vec+ #:vec-minus #:x #:y #:z #:inverse #:scalar/ #:scalar* #:angle-between #:dot-product #:cross-product #:cartesian #:square-magnitude #:zero-cartesian))

(in-package :starshot/vector)

(defclass cartesian ()
  ((x
    :initarg :x
    :accessor x)
   (y
    :initarg :y
    :accessor y)
   (z
    :initarg :z
    :accessor z)))

(defun make-cartesian (x y z)
  (declare (type real x y z))
  (make-instance 'cartesian :x x :y y :z z))

(defun zero-cartesian ()
  (make-instance 'cartesian :x 0 :y 0 :z 0))

(defmethod print-object ((c cartesian) out)
  (print-unreadable-object (c out :type t)
    (format out "~a ~a ~a" (x c) (y c) (z c))))

;; Take an object and a list of accessors and create a function that maps over the accessors

(defun accessor-map (obj accessors fn)
  (mapcar (compose (callable obj) fn) accessors))

(defmethod map-cartesian ((c cartesian) fn)
  (accessor-map c (list #'x #'y #'z) fn))

(defvar square (partialr #'expt 2))

(defmethod square-magnitude ((c cartesian))
  (apply #'+
         (map-cartesian c square)))

(defmethod magnitude ((c cartesian))
  (funcall (compose #'square-magnitude #'sqrt) c))

(defmethod normalize ((c cartesian))
  (apply #'make-cartesian
         (map-cartesian c (partialr #'/ (magnitude c)))))

(defmethod scalar* (s (c cartesian))
  (apply #'make-cartesian
         (map-cartesian c (partial #'* s))))

(defmethod scalar/ (s (c cartesian))
  (scalar* (/ 1 s) c))

(defmethod inverse ((c cartesian))
  (apply #'make-cartesian
         (map-cartesian c (partial #'- 0))))

(defmethod vec+ ((c1 cartesian) (c2 cartesian))
  (make-cartesian (+ (x c1) (x c2))
                  (+ (y c1) (y c2))
                  (+ (z c1) (z c2))))

(defmethod vec-minus ((c1 cartesian) (c2 cartesian))
  (vec+ c1 (inverse c2)))

(defmethod component-product ((c1 cartesian) (c2 cartesian))
  (make-cartesian (* (x c1) (x c2))
                  (* (y c1) (y c2))
                  (* (z c1) (z c2))))

(defmethod sum-components ((c cartesian))
  (apply #'+ (map-cartesian c #'identity)))

(defmethod dot-product ((c1 cartesian) (c2 cartesian))
  (funcall (compose #'component-product #'sum-components) c1 c2))

(defmethod cross-product ((c1 cartesian) (c2 cartesian))
  (make-cartesian (- (* (y c1) (z c2)) (* (z c1) (y c2)))
                  (- (* (z c1) (x c2)) (* (x c1) (z c2)))
                  (- (* (x c1) (y c2)) (* (y c1) (x c2)))))


(defmethod angle-between ((c1 cartesian) (c2 cartesian))
  (funcall (compose #'dot-product #'acos) (normalize c1) (normalize c2)))
