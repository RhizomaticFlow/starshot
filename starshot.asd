(defsystem #:starshot
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("closer-mop" "tactile" "lispbuilder-sdl")
  :serial T
  :components ((:module "src"
                :components
                ((:file "mop")
                 (:file "vector")
                 (:file "particle")
                 (:file "graphics")
                 (:file "p0"))))
  :description ""
  :in-order-to ((test-op (test-op "starshot/tests"))))

(defsystem #:starshot/tests
  :author ""
  :license ""
  :depends-on ("starshot"
               "fiveam")
  :serial T
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:file "main"))))
  :description "Test system for starshot"
  :perform (test-op (op s) (symbol-call :starshot/tests '#:debug-starshot-suite)))
