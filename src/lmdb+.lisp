(in-package :lmdb+)

(cl-reexport:reexport-from ':lmdb)

(setf (symbol-function 'get) #'g3t)
