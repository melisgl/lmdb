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
    (finishes
      (lmdb:open-environment env))
    (finishes
      (lmdb:environment-statistics env))
    (finishes
      (lmdb:environment-info env))
    (finishes
      (lmdb:close-environment env)))
  (clean))

(test database
  (let ((env))
    (finishes
      (setf env (lmdb:make-environment +env-directory+)))
    (lmdb:with-environment (env)
      (let ((txn))
        (finishes
         (setf txn (lmdb:make-transaction env)))
        (finishes
         (lmdb:begin-transaction txn))
        (let ((db))
          (finishes
           (setf db (lmdb:make-database txn "db"
                                        :create t)))
          (is-true
           (lmdb::database-create-p db))
          (finishes
            (lmdb:open-database db))
          (finishes
            (lmdb:close-database db))))))
  (clean))

(test value
  (loop for val in (list 1
                         123
                         -100000000
                         "string"
                         "string with unicode 㐎 㐏 㐐 㐑 㐒 㐓 㐔")
        do
    (let ((value (lmdb:make-value val)))
      (is-true
       (lmdb:value-p value))
      (is-true
       (integerp (lmdb:value-size value)))
      (is-true
       (vectorp (lmdb:value-data value))))))

(defun run-tests ()
  (run! 'tests))
