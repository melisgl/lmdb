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
  (let ((env (lmdb:make-environment +env-directory+)))
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

#|
(test memory-leaks
  (finishes
    (dotimes (i 1000000)
      (let ((env (lmdb:make-environment +env-directory+)))
        (lmdb:open-environment env)
        (lmdb:close-environment env)))))
|#

(test values
  (loop for val in (list 1
                         123
                         -100000000
                         "string"
                         "string with unicode 㐎 㐏 㐐 㐑 㐒 㐓 㐔")
        do
    (let ((value (lmdb::make-value val)))
      (is-true
       (lmdb::value-p value))
      (is-true
       (integerp (lmdb::value-size value)))
      (is-true
       (vectorp (lmdb::value-data value)))
      (lmdb::with-val (raw-val val)
        (is
         (equal (cffi:pointer-address
                 (cffi:foreign-slot-value raw-val
                                          '(:struct lmdb.low:val)
                                          'lmdb.low:mv-size))
                (lmdb::value-size value)))
        (loop for i from 0 to (1- (lmdb::value-size value)) do
          (is
           (equal (cffi:mem-aref (cffi:foreign-slot-value raw-val
                                                          '(:struct lmdb.low:val)
                                                          'lmdb.low:mv-data)
                                 :unsigned-char
                                 i)
                  (elt (lmdb::value-data value) i))))))))

(test queries
  (let ((env (lmdb:make-environment +env-directory+)))
    (lmdb:with-environment (env)
      (let ((txn (lmdb:make-transaction env)))
        (finishes
          (lmdb:begin-transaction txn))
        (let ((db (lmdb:make-database txn "db")))
          (lmdb:with-database (db)
            (finishes
              (lmdb:put db 1 2))
            (let ((vec (lmdb:get db 1)))
              (is (equal (length vec) 1))
              (is (equal (elt vec 0) 2)))))))))

(defun run-tests ()
  (run! 'tests))
