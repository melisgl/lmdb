(defpackage lmdb/test
  (:shadowing-import-from #:lmdb+ #:get)
  (:use #:cl #:lmdb)
  (:export #:test))
