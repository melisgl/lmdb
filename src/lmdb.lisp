(in-package :cl-user)
(defpackage lmdb
  (:use :cl)
  (:export :version-string
           :environment
           :make-environment
           :open-environment
           :environment-statistics
           :environment-info)
  (:documentation "The high-level LMDB interface."))
(in-package :lmdb)

;;; Library

(cffi:define-foreign-library liblmdb
  (:darwin (:or "liblmdb.dylib" "liblmdb.1.dylib"))
  (:unix  (:or "liblmdb.so" "liblmdb.so.0.0.0"))
  (:win32 "liblmdb.dll")
  (t (:default "liblmdb")))

(cffi:use-foreign-library liblmdb)

;;; Interface

(defparameter +permissions+ #o664
  "The Unix permissions to use for the database.")

;; Some error codes
(defparameter +enoent+ 2)
(defparameter +eacces+ 13)
(defparameter +eagain+ 3406)

(defun version-string ()
  "Return the version string."
  (format nil "~D.~D.~D"
          lmdb.low:+version-major+
          lmdb.low:+version-minor+
          lmdb.low:+version-patch+))

(defclass environment ()
  ((env :accessor environment-env
        :initarg :env
        :documentation "The pointer to the environment handle pointer.")
   (directory :reader environment-directory
              :initarg :directory
              :documentation "The directory where environment files are stored."))
  (:documentation "Environment handle."))

(defun make-environment (directory)
  "Create an environment object."
  (let ((instance (make-instance 'environment
                                 :env (cffi:foreign-alloc :pointer)
                                 :directory directory)))
    (unless (= (lmdb.low:env-create (environment-env instance)) 0)
      (error "Error creating environment object."))
    instance))

(defun environment-handle (environment)
  "Return the environment handle."
  (cffi:mem-ref (environment-env environment) :pointer))

(defun open-environment (environment)
  "Open the environment connection."
  (assert (uiop:directory-pathname-p (environment-directory environment)))
  (let ((return-code (lmdb.low:env-open (environment-handle environment)
                                        (namestring (environment-directory environment))
                                        0
                                        (cffi:make-pointer +permissions+))))
    (alexandria:switch (return-code)
      (lmdb.low:+version-mismatch+
       (error "Version mismatch: the client version is different from the environment version."))
      (lmdb.low:+invalid+
       (error "Data corruption: the environment header files are corrupted."))
      (+enoent+
       (error "The environment directory doesn't exist."))
      (+eacces+
       (error "The user doesn't have permission to use the environment directory."))
      (+eagain+
       (error "The environment files are locked by another process."))
      (0
       ;; Success
       )
      (t
       (error "Unknown error code."))))
  environment)

(defun environment-statistics (environment)
  "Return statistics about the environment."
  (cffi:with-foreign-object (stat '(:struct lmdb.low:stat))
    (lmdb.low:env-stat (environment-handle environment)
                       stat)
    (macrolet ((slot (slot)
                 `(cffi:foreign-slot-value stat
                                           '(:struct lmdb.low:stat)
                                           ',slot)))
      (list :page-size (slot lmdb.low:ms-psize)))))

(defun environment-info (environment)
  "Return information about the environment."
  (cffi:with-foreign-object (info '(:struct lmdb.low:envinfo))
    (lmdb.low:env-info (environment-handle environment)
                       info)
    (macrolet ((slot (slot)
                 `(cffi:foreign-slot-value info
                                           '(:struct lmdb.low:envinfo)
                                           ',slot)))
      (list :map-address (cffi:pointer-address (slot lmdb.low:me-mapaddr))
            :map-size (cffi:pointer-address (slot lmdb.low:me-mapsize))))))
