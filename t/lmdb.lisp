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

(test environment
  (let ((env))
    (finishes
      (setf env (lmdb:make-environment +env-directory+)))
    (lmdb:with-environment (env)
      (finishes
        (lmdb:open-environment env))
      (finishes
        (lmdb:environment-statistics env))
      (finishes
        (lmdb:environment-info env)))))

(test database
  (let ((env))
    (finishes
      (setf env (lmdb:make-environment +env-directory+)))
    (lmdb:with-environment (env)
      (finishes
        (lmdb:open-environment env))
      (let ((txn))
        (finishes
          (setf txn (lmdb:make-transaction env)))
        (finishes
          (lmdb:begin-transaction txn))
        (let ((db))
          (finishes
            (setf db (lmdb:make-database "db")))
          (finishes
            (lmdb:open-database db txn)))))))


(defun run-tests ()
  (run! 'tests))
