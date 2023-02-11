(defpackage starshot/mop
  (:use :cl)
  (:import-from :starshot/combinators :compose :juxt)
  (:export #:get-classname #:get-slot-names #:slots->plist #:update-plist #:update-object #:update-object* #:close-enough-p #:self))

(in-package :starshot/mop)

(defvar get-classname (compose #'class-of #'class-name))

(defun get-slot-names (object)
  (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots (class-of object))))

(defun slots->plist (object)
  (mapcan (lambda (slot-name)
            (list (intern (symbol-name slot-name) "KEYWORD") (slot-value object slot-name)))
          (get-slot-names object)))

(defun update-plist (plist indicator new-value)
    (let ((other-properties nil))
      (loop while plist
            for property = (pop plist)
            for value = (pop plist)
            when (eq property indicator)
            do (return-from update-plist (list* property new-value
                                                (append other-properties plist)))
            else do (push value other-properties)
                    (push property other-properties))
      (list* indicator new-value other-properties)))

(defun update-object (object key val)
  (apply #'make-instance (cons (funcall get-classname object)
                               (update-plist (slots->plist object) key val))))

(defmacro update-object* (self clauses)
  "Sequential update of an object. self = an object, clauses = plist of slot-name as keyword and function that takes object as a value"
  (reduce (lambda (acc clause)
            `(let ((self ,acc))
               (update-object self ,(first clause) ,(second clause))))
          clauses
          :initial-value self))

;; (defclass c () ((c :initarg :c :accessor c) (d :initarg :d :accessor d)))

;; (format t "~a" (macroexpand '(update-object* (make-instance 'c :c 0 :d 0) ((:c 1) (:c (+ (c self) 1)) (:d (+ (c self) 2)) (:d 1)))))
