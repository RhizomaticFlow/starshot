(in-package :starshot/tests)

;; NOTE: To run this test file, execute `(asdf:test-system :starshot)' in your Lisp.

(fiveam:def-suite starshot-suite)
(fiveam:in-suite starshot-suite)

(defun test-starshot-suite ()
  (fiveam:run! 'starshot-suite))

(defun debug-starshot-suite ()
  (fiveam:debug! 'starshot-suite))

(defun close-enough-p (f1 f2 &optional (epsilon 0.01))
  (declare (type real f1 f2 epsilon))
  (let ((delta (* (abs f1) epsilon)))
    (<= (- f1 delta)
        f2
        (+ f1 delta))))

(fiveam:test update-object-test
             (fiveam:is
              (= 1 (vec:x (update-object (make-cartesian 0 3 0) :x 1)))))

(fiveam:test update-object-anaphora-test
  (let ((updated (update-object* (make-cartesian 0 3 0)
                                 ((:x 1) (:y (+ 1 (vec:y mop::self)))))))
    (fiveam:is (= 4 (vec:y updated)))))

(fiveam:test vector-operations
  (let ((vec-1 (make-cartesian 1 2 3))
        (vec-2 (make-cartesian 4 5 6)))
    (fiveam:is (= 12 (vec:square-magnitude (make-cartesian 2 2 2))))
    (fiveam:is (eql (vec:magnitude (vec:vec-minus vec-2 vec-1))
                    (vec:magnitude (vec:vec+ vec-2 (vec:inverse vec-1)))))
    (fiveam:is (= (vec:magnitude vec-1) (sqrt 14)))))

(fiveam:test product-tests
  (let ((vec-1 (make-cartesian 3 0 0))
        (vec-2 (make-cartesian 0 3 0)))
    (fiveam:is (close-enough-p (/ pi 2) (vec:angle-between vec-1 vec-2)))))

(fiveam:test cartesian-updates-on-particle-position
  (fiveam:is (= 1 (vec:y (p:calculate-new-position (make-particle (make-cartesian 0 0 0) (make-cartesian 0 1 0) (make-cartesian 0 0 0) 0.995 1/3 3 t) 1)))))

(fiveam:test cartesian-updates-on-particle-velocity
  (fiveam:is (= 1.9 (vec:y (p:calculate-new-velocity (make-particle (make-cartesian 0 0 0) (make-cartesian 0 1 0) (make-cartesian 0 1 0) 0.9 1/3 3 t) 1)))))

(fiveam:test integration
  (fiveam:is (= 1.9
                (vec:y (p:p
                        (p:integrate
                         (make-particle (make-cartesian 0 0 0) (make-cartesian 0 1 0) (make-cartesian 0 1 0) 0.9 1 3 t)
                         1
                         (make-cartesian 0 1 0)))))))

(fiveam:test collision-determination
  (fiveam:is (p:collision?
              (make-particle (make-cartesian 10 0 0) (make-cartesian 0 0 0) (make-cartesian 0 0 0) 0.9 1/3 5 t)
              (make-particle (make-cartesian 0 0 0) (make-cartesian 0 0 0) (make-cartesian 0 0 0) 0.9 1/3 5 t)))
  (fiveam:is (not (p:collision?
                   (make-particle (make-cartesian 10.1 0 0) (make-cartesian 0 0 0) (make-cartesian 0 0 0) 0.9 1/3 5 t)
                   (make-particle (make-cartesian 0 0 0) (make-cartesian 0 0 0) (make-cartesian 0 0 0) 0.9 1/3 5 t)))))

(fiveam:test collision-calculation
             (fiveam:is
              (equal (list 3/4 15/4) 
               (second (p:calculate-collision
                       (make-particle (make-cartesian 0 0 0)
                                      (make-cartesian 0 3 0)
                                      (make-cartesian 0 0 0)
                                      0.9 1/5 5 t)
                       (make-particle (make-cartesian 0 0 0)
                                      (make-cartesian 0 0 0)
                                      (make-cartesian 0 0 0)
                                      0.9 1/3 5 t))))))

;;
;; p0
;; 

(fiveam:test iterate-state
  (let ((state1
          (make-p0 0.01
                   (list
                    (make-particle (make-cartesian 0 0 0)
                                   (make-cartesian 0 3 0)
                                   (make-cartesian 0 0 0)
                                   0.9 1/5 5 t)
                    (make-particle (make-cartesian 0 0 0)
                                   (make-cartesian 0 0 0)
                                   (make-cartesian 0 0 0)
                                   0.9 1/3 5 t)))))
    (iterate-state state)
    ;; (fiveam:is (equal (list 1 2) (iterate-state state)))
    (fiveam:is (equal (list 0.74921024 3.746051)
                      (mapcar
                       (compose #'p:v #'vec:y)
                       (p0:particles state))))
    (fiveam:is
     (= 0.03746051
        (vec:y (p:p (second (p0:particles state))))))))
