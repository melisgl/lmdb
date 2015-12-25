# LMDB

Common Lisp bindings to [LMDB](http://symas.com/mdb/).

# Overview

LMDB is really fast. [Here's][sql] how much faster SQLite got when they ported
the B-tree code to use LMDB.

# Usage

~~~lisp
;; Create an environment
(let ((env (lmdb:make-environment #p"/some/directory/")))
  (lmdb:with-environment (env)
    ;; Create a transaction
    (let ((txn (lmdb:make-transaction env)))
      (lmdb:begin-transaction txn))
      ;; Create a database access object
      (let ((db (lmdb:make-database txn "db")))
        (lmdb:with-database (db)
          (lmdb:put db 1 2))
          (let ((vec (lmdb:get db 1)))
            (print vec)))))
~~~

# License

Copyright (c) 2015 Fernando Borretti

Licensed under the MIT License.

[sql]: https://github.com/LMDB/sqlightning
