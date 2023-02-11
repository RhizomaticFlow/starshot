(defpackage starshot/combinators
  (:use :cl)
  (:export #:compose #:juxt #:callable #:reduce-apply #:partial #:partialr #:reduce-apply #:-> #:->>))

(in-package :starshot/combinators)

(defun partial (fn &rest args)
  (lambda (&rest args2)
    (apply fn (append args args2))))

(defun partialr (fn &rest args)
  (lambda (&rest args2)
    (apply fn (append args2 args))))

(defun compose (&rest fns)
  (lambda (&rest args)
    (first
     (reduce
      (lambda (acc fn)
        (list (apply fn acc)))
      fns
      :initial-value args))))

(defun juxt (&rest fns)
  (lambda (x)
    (mapcar (lambda (fn)
              (funcall fn x))
            fns)))

(defun flip (fn)
  (lambda (x y)
    (funcall fn y x)))

(defmacro -> (&body body)
  (reduce
   (lambda (acc clause)
     `(,(first clause) ,acc ,@(rest clause)))
   body))

(defmacro ->> (&body body)
  (reduce
   (lambda (acc clause)
     `(,(first clause) ,@(rest clause) ,acc))
   body))

(defun callable (x)
  (lambda (fn)
    (funcall fn x)))

(defun reduce-apply (&rest arg-lists)
  (reduce #'apply arg-lists))
