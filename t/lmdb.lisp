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

(defun clean ()
  (uiop:delete-directory-tree +env-directory+ :validate t))

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
       (lmdb:environment-info env))))
  (clean))

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
           (setf db (lmdb:make-database env "db"
                                        :create t)))
          (is-true
           (lmdb::database-create-p db))
          (finishes
           (lmdb:open-database db txn))))))
  (clean))

(defun run-tests ()
  (run! 'tests))
