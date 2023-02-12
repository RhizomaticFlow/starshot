(defpackage starshot/vector
  (:use :cl)
  (:import-from :tactile #:compose #:juxt #:callable #:partialr #:partial #:reduce-apply)
  (:export #:make-cartesian #:magnitude #:vec+ #:vec-minus #:x #:y #:z #:inverse #:scalar/ #:scalar* #:angle-between #:dot-product #:cross-product #:cartesian #:square-magnitude))

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

(defvar accessor-map
  (lambda (obj)
    (lambda (&rest accessors)
      (lambda (&rest fns)
        (mapcar (apply #'compose (cons (callable obj) fns)) accessors)))))

(defmethod map-cartesian ((c cartesian))
  (reduce-apply accessor-map (list c) (list #'x #'y #'z)))

(defvar square (partialr #'expt 2))

(defmethod square-magnitude ((c cartesian))
  (apply #'+
         (funcall (map-cartesian c) square)
         ))

(defmethod magnitude ((c cartesian))
  (funcall (compose #'square-magnitude #'sqrt) c))

(defmethod normalize ((c cartesian))
  (apply #'make-cartesian
         (funcall (map-cartesian c) (partialr #'/ (magnitude c)))))

(defmethod scalar* (s (c cartesian))
  (apply #'make-cartesian
         (funcall (map-cartesian c) (partial #'* s))))

(defmethod scalar/ (s (c cartesian))
  (scalar* (/ 1 s) c))

(defmethod inverse ((c cartesian))
  (apply #'make-cartesian
         (funcall (map-cartesian c) (partial #'- 0))))

(defmethod vec+ ((c1 cartesian) (c2 cartesian))
  (apply #'make-cartesian (mapcar #'+
                          (funcall (map-cartesian c1))
                          (funcall (map-cartesian c2)))))

(defmethod vec-minus ((c1 cartesian) (c2 cartesian))
  (vec+ c1 (inverse c2)))

(defmethod component-product ((c1 cartesian) (c2 cartesian))
  (apply #'make-cartesian (mapcar #'*
                                  (funcall (map-cartesian c1))
                                  (funcall (map-cartesian c2)))))

(defmethod sum-components ((c cartesian))
  (apply #'+ (funcall (map-cartesian c))))

(defmethod dot-product ((c1 cartesian) (c2 cartesian))
  (funcall (compose #'component-product #'sum-components) c1 c2))

(defmethod cross-product ((c1 cartesian) (c2 cartesian))
  (make-cartesian (- (* (y c1) (z c2)) (* (z c1) (y c2)))
                  (- (* (z c1) (x c2)) (* (x c1) (z c2)))
                  (- (* (x c1) (y c2)) (* (y c1) (x c2)))))


(defmethod angle-between ((c1 cartesian) (c2 cartesian))
  (funcall (compose #'dot-product #'acos) (normalize c1) (normalize c2)))
