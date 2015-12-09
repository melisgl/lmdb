(in-package :cl-user)
(defpackage lmdb
  (:use :cl)
  (:documentation "The high-level LMDB interface."))
(in-package :lmdb)

(defun version-string ()
  "Return the version string."
  (format nil "~D.~D.~D"
          lmdb.low:+version-major+
          lmdb.low:+version-minor+
          lmdb.low:+version-patch+))
