(in-package :cl-user)
(defpackage lmdb-test
  (:use :cl :fiveam)
  (:export :run-tests))
(in-package :lmdb-test)

(def-suite tests
  :description "lmdb tests.")
(in-suite tests)

(defparameter +env-directory+
  (asdf:system-relative-pathname :lmdb #p"t/env/"))

(test connect
  (let ((env))
    ;; Create directory
    (ensure-directories-exist +env-directory+)
    (finishes
     (setf env (lmdb:make-environment +env-directory+)))
    (finishes
      (lmdb:open-environment env))
    (finishes
     (lmdb:environment-statistics env))
    (finishes
     (lmdb:environment-info env))))

(defun run-tests ()
  (run! 'tests))
