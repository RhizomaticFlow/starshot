(defpackage starshot/tests
  (:use :cl :fiveam)
  (:import-from :tactile :compose :juxt)
  (:import-from :starshot/mop :update-object :update-object* :get-classname)
  (:import-from :starshot/vector :make-cartesian)
  (:import-from :starshot/particle :make-particle)
  (:import-from :starshot/p0 :make-p0 :iterate-state)
  (:local-nicknames (:comb :tactile)
                    (:mop :starshot/mop)
                    (:vec :starshot/vector)
                    (:p :starshot/particle)
                    (:p0 :starshot/p0)))
