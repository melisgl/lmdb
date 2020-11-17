(in-package :lmdb)

(in-readtable pythonic-string-syntax)

;;;; Foreign stuff

(cffi:define-foreign-library liblmdb
  (:darwin (:or "liblmdb.dylib" "liblmdb.1.dylib"))
  (:unix  (:or "liblmdb.so" "liblmdb.so.0" "liblmdb.so.0.0.0"))
  (:win32 "liblmdb.dll")
  (t (:default "liblmdb")))

(cffi:use-foreign-library liblmdb)

(defun errno-value (keyword)
  (cffi:foreign-enum-value 'osicat-posix::errno-values keyword))

(defparameter +enoent+ (errno-value :enoent))
(defparameter +eagain+ (errno-value :eagain))
(defparameter +eacces+ (errno-value :eacces))
(defparameter +einval+ (errno-value :einval))

;;; Singleton to reduce consing.
(alexandria:define-constant +null-pointer+ (cffi:null-pointer)
  :test #'cffi:pointer-eq)


;;;; Word-aligned pointer <-> FIXNUM
;;;;
;;;; To avoid consing CFFI:FOREIGN-POINTER (e.g.
;;;; SB-SYS:SYSTEM-AREA-POINTER) objects, we make use of the fact that
;;;; word-aligned pointers (such as those returned by calloc(), which
;;;; by contract must be "suitably aligned for any built-in type")
;;;; have a few zero low bits. Then we assume that shifting the
;;;; aligned pointer's address by 2 or 3 bits gives us a positive
;;;; fixnum.

#+32-bit
(eval-when (:compile-toplevel :load-toplevel :load-toplevel)
  (assert (typep (1- (expt 2 32)) '(unsigned-byte 32)))
  (assert (typep (ash (1- (expt 2 32)) -2) 'fixnum))
  (alexandria:define-constant +n-word-bits+ 32)
  (alexandria:define-constant +n-alignment-bits+ 2))
#+64-bit
(eval-when (:compile-toplevel :load-toplevel :load-toplevel)
  (assert (typep (1- (expt 2 64)) '(unsigned-byte 64)))
  (assert (typep (ash (1- (expt 2 64)) -3) 'fixnum))
  (alexandria:define-constant +n-word-bits+ 64)
  (alexandria:define-constant +n-alignment-bits+ 3))

(declaim (inline aligned-pointer-to-fixnum))
(defun aligned-pointer-to-fixnum (ptr)
  (declare (type cffi:foreign-pointer)
           (optimize speed))
  (let ((address (cffi:pointer-address ptr)))
    ;; Check that the low bits are 0.
    (assert (not (logtest address #.(1- (expt 2 +n-alignment-bits+)))))
    (the fixnum (ash address (- +n-alignment-bits+)))))

(declaim (inline fixnum-to-aligned-pointer))
(defun fixnum-to-aligned-pointer (fixnum)
  (declare (type fixnum fixnum))
  (cffi:make-pointer (ldb (byte #.+n-word-bits+ 0)
                          (ash fixnum +n-alignment-bits+))))


;;;; Signal handling

;;; Only used for self-documenting.
(defmacro async-signal-safe (&body body)
  `(progn ,@body))

#+allegro
(defmacro without-interrupts (&body body)
  `(excl:with-delayed-interrupts ,@body))

#+cmucl
(defmacro without-interrupts (&body body)
  `(sys:without-interrupts ,@body))

#+lispworks
(defmacro without-interrupts (&body body)
  `(mp:with-interrupts-blocked ,@body))

#+sbcl
(progn
  (defmacro without-interrupts (&body body)
    `(sb-sys:without-interrupts
       (sb-sys:allow-with-interrupts
         ,@body)))
  (defmacro with-interrupts (&body body)
    `(sb-sys:with-interrupts ,@body)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; This is very bad. See @SAFETY.
  (unless (fboundp 'without-interrupts)
    (error "WITHOUT-INTERRUPTS not implemented."))
  ;; This is milder., but it means that WITH-TXN's body will not be
  ;; interruptible.
  (unless (fboundp 'with-interrupts)
    (warn "~@<WITH-INTERRUPTS not implemented. Some code, most prominently ~
          the body of WITH-TXN, WITH-CURSOR will not be interruptible.~:@>")
    (defmacro with-interrupts (&body body)
      `(progn ,@body))))

;;; Recompile with these when doing statistical profiling that relies
;;; on signals. Or when feeling brave.
#+nil
(progn
  (defmacro without-interrupts (&body body)
    `(progn ,@body))
  (defmacro with-interrupts (&body body)
    `(progn ,@body)))


;;;; Utilities

(defmacro assert-error (condition-type &body body)
  (alexandria:with-gensyms (got-it)
    `(let ((,got-it nil))
       (unwind-protect
            (handler-case (progn ,@body)
              (,condition-type ()
                (setq ,got-it t)))
         (assert ,got-it () "Didn't get expected ~S." ',condition-type)))))


(defsection @lmdb-manual (:title "LMDB Manual")
  (lmdb asdf:system)
  (@lmdb-links section)
  (@lmdb-introduction section)
  (@design-and-implementation section)
  (@version section)
  (@environments section)
  (@transactions section)
  (@databases section)
  (@encodings section)
  (@basic-operations section)
  (@cursors section)
  (@conditions section))

(defsection @lmdb-links (:title "Links")
  "Here is the [official repository](https://github.com/antimer/lmdb)
  and the [HTML
  documentation](http://melisgl.github.io/mgl-pax-world/lmdb-manual.html)
  for the latest version.")

(defsection @lmdb-introduction (:title "Introduction")
  """[LMDB](http://www.lmdb.tech/doc/), the Lightning Memory-mapped
  Database, is an [ACID](https://en.wikipedia.org/wiki/ACID) key-value
  database with
  [MVCC](https://en.wikipedia.org/wiki/Multiversion_concurrency_control).
  It is a small C library ("C lmdb" from now on), around which LMDB is
  a Common Lisp wrapper. LMDB covers most of C lmdb's functionality,
  has a simplified API, much needed @SAFETY checks, and comprehensive
  documentation.

  Compared to other key-value stores, LMDB's distuingishing features
  are:

  - Transactions span multiple keys.

  - Embedded. It has no server but can be used concurrently not only
    by multiple threads but by multiple OS processes, too.

  - Extremely high read performance: millions of transactions per
    second.

  - Very low maintenance.

  Other notable things:

  - With its default - the most durable - settings, it has average
    write performance, which is bottlenecked by `fsync()`.

  - Readers don't block readers or writers, but there is at most one
    writer at a time.

  - Extremely simple, crash-proof design.

  - The entire database (called *environment*) is backed by a single
    memory-mapped file, with a
    [copy-on-write](https://en.wikipedia.org/wiki/Copy-on-write)
    [B+ tree](https://en.wikipedia.org/wiki/B%2B_tree).

  - No transaction log.

  - It is very much like [Berkeley
    DB](https://en.wikipedia.org/wiki/Berkeley_DB) done right, without
    the fluff and much improved administration.

  Do read the [Caveats](http://www.lmdb.tech/doc/), though. On the
  Lisp side, this library __will not work with virtual threads__
  because LMDB's write locking is tied to native threads.

  Using LMDB is easy:

  ```
  (with-temporary-env (*env*)
    (let ((db (get-db "test" :if-does-not-exist :create)))
      (with-txn (:write t)
        (put db "k1" #(2 3))
        (print (g3t db "k1")) ; => #(2 3)
        (del db "k1"))))
  ```

  More typically, the environment and databases are opened once so
  that multiple threads and transactions can access them:

  ```
  (defvar *test-db*)

  (unless *env*
    (setq *env* (open-env "/tmp/lmdb-test-env/" :if-does-not-exist :create))
    (setq *test-db* (get-db "test" :if-does-not-exist :create
                                   :value-encoding :utf-8)))

  (with-txn (:write t)
    (put *test-db* 1 "hello")
    (print (g3t *test-db* 1)) ; => "hello"
    (del *test-db* 1))
  ```

  Note how :VALUE-ENCODING sneaked in above. This was so to make G3T
  return a string instead of an octet vector.

  LMDB treats keys and values as opaque byte arrays to be hung on a B+
  tree, and only requires a comparison function to be defined over
  keys. LMDB knows how to serialize the types `(UNSIGNED-BYTE 64)` and
  STRING (which are often used as keys so sorting must work as
  expected). Serialization of the rest of the datatypes is left to the
  client. See @ENCODINGS for more.
  """)

(defsection @design-and-implementation (:title "Design and implementation")
  (@safety section)
  (@deviations-from-the-lmdb-api section))

(defsection @safety (:title "Safety")
  "The lmdb C API trusts client code to respect its rules. Being C,
  managing object lifetimes is the biggest burden. There are also
  rules that are documented, but not enforced. This Lisp wrapper tries
  to enforce these rules itself and manage object lifetimes in a safe
  way to avoid data corruption. How and what it does is described in
  the following.

  ##### Environments

  - OPEN-ENV checks that the same path is not used in multiple open
    environments to prevent locking issues documented in
    [Caveats](http://www.lmdb.tech/doc/).

  - CLOSE-ENV waits until all @ACTIVE-TRANSACTIONs are finished before
    actually closing the environment. Alternatively, if OPEN-ENV was
    called with :SYNCHRONIZED NIL, to avoid the overhead of
    synchronization, the environment is closed only when garbage
    collected.

  ##### Transactions

  - Checks are made to detect illegal operations on parent
    transactions (see LMDB-ILLEGAL-ACCESS-TO-PARENT-TXN-ERROR).

  - Access to closed transactions is reliably detected.

  - C LMDB allows read transactions to be used in multiple threads.
    The synchronization cost of performing this safely (i.e. without
    risking access to closed and freed transaction objects) is
    significant so this is not supported.

  ##### Databases

  - [mdb_dbi_open()](http://www.lmdb.tech/doc/group__mdb.html#gac08cad5b096925642ca359a6d6f0562a)
    is wrapped by GET-DB in a transaction and is protected by a mutex
    to comply with C lmdb's requirements:

           A transaction that opens a database must finish (either
           commit or abort) before another transaction may open it.
           Multiple concurrent transactions cannot open the same
           database.

  - [mdb_dbi_close()](http://www.lmdb.tech/doc/group__mdb.html#ga52dd98d0c542378370cd6b712ff961b5)
    is too dangerous to be exposed as explained in the GET-DB
    documentation.

  - For similar reasons, DROP-DB is wrapped in WITH-ENV.

  - [mdb_env_set_mapsize()](http://www.lmdb.tech/doc/group__mdb.html#gaa2506ec8dab3d969b0e609cd82e619e5),
    [mdb_env_set_max_readers()](http://www.lmdb.tech/doc/group__mdb.html#gae687966c24b790630be2a41573fe40e2),
    and [mdb_env_set_maxdbs()](http://www.lmdb.tech/doc/group__mdb.html#gaa2fc2f1f37cb1115e733b62cab2fcdbc)
    are only available through OPEN-ENV because they either require that
    there are no write transactions or do not work on open environments.

  ##### Cursors

  - As even read transactions are restricted to a single thread, so
    are cursors. Using a cursor from a thread other than the one in
    which it was created (i.e. the thread of its transaction) raises
    LMDB-CURSOR-THREAD-ERROR. In return for this restriction, access
    to cursors belonging to closed transactions is reliably detected.

  ##### Signal handling

  The C lmdb library handles system calls being interrupted (`EINTR`
  and `EAGAIN`), but unwinding the stack from interrupts in the middle
  of LMDB calls can leave the in-memory data structures such as
  transactions inconsistent. If this happens, their further use risks
  data corruption. For this reason, calls to LMDB are performed with
  interrupts disabled. For SBCL, this means SB-SYS:WITHOUT-INTERRUPTS.
  It is an error when compiling LMDB if an equivalent facility is not
  found in the Lisp implementation. A warning is signalled if no
  substitute is found for SB-SYS:WITH-INTERRUPTS because this makes
  the body of WITH-TXN and WITH-CURSOR uninterruptible.

  Operations that do not modify the database (G3T, CURSOR-FIRST,
  CURSOR-VALUE and similar) are async unwind safe, and for performance
  they are called without the above provisions.

  Note that the library is not reentrant, so don't call LMDB from
  signal handlers.")

(defsection @deviations-from-the-lmdb-api (:title "Deviations from C lmdb API")
  "The following are the most prominent deviations and omissions from
  the C lmdb API in addition to those listed in @SAFETY.

  ##### Environments

  - [mdb_reader_list()](http://www.lmdb.tech/doc/group__mdb.html#ga8550000cd0501a44f57ee6dff0188744)
    is not implemented.

  - [mdb_env_copy](http://www.lmdb.tech/doc/group__mdb.html#ga5d51d6130325f7353db0955dbedbc378)
    and its close kin are not yet implemented.

  ##### Transactions

  - Read-only WITH-TXNs are turned into noops when \"nested\" (unless
    IGNORE-PARENT).

  ##### Databases

  - [mdb_set_compare()](http://www.lmdb.tech/doc/group__mdb.html#ga68e47ffcf72eceec553c72b1784ee0fe)
    and [mdb_set_dupsort()](http://www.lmdb.tech/doc/group__mdb.html#gacef4ec3dab0bbd9bc978b73c19c879ae)
    are not exposed. If this is needed, implement a foreign comparison
    function and call LIBLMDB:SET-COMPARE or LIBLMDB:SET-DUPSORT
    directly or perhaps change the encoding of the data.

  - Working with multiple contiguous values with DUPFIXED is not yet
    implemented. This functionality would belong in PUT, CURSOR-PUT,
    CURSOR-NEXT and CURSOR-VALUE.

  - PUT, CURSOR-PUT do not support the
    [RESERVE](http://www.lmdb.tech/doc/group__mdb__put.html#gac0545c6aea719991e3eae6ccc686efcc)
    flag.")


(defsection @conditions (:title "Conditions")
  (lmdb-serious-condition condition)
  (lmdb-error condition)
  (@lmdb-error-code-conditions section)
  (@additional-conditions section))

;;; C lmdb uses -30600 to -30799. We use -30599 and up.
(defparameter +cursor-uninitialized+ -30599)
(defparameter +cursor-thread+ -30598)
(defparameter +txn-read-only+ -30597)
(defparameter +illegal-access-to-parent-txn+ -30596)

(defun strerror (error-code)
  (alexandria:switch (error-code)
    (+cursor-uninitialized+ "Cursor is not initialized.")
    (+cursor-thread+ (format nil "Cursor was accessed from a thread other ~
                                 than the one in which it was created."))
    (+txn-read-only+ "Attempt to write in a read-only transaction.")
    (+illegal-access-to-parent-txn+ "Illegal access to parent transaction.")
    (t (liblmdb:strerror error-code))))

(define-condition lmdb-serious-condition (serious-condition)
  ((error-code :initarg :error-code :reader lmdb-error-error-code)
   (control-string :initarg :control-string :reader lmdb-error-control-string)
   (format-args :initarg :format-args :reader lmdb-error-format-args))
  (:report (lambda (condition stream)
             (with-slots (error-code control-string format-args) condition
               (let ((errorp (and error-code (not (zerop error-code)))))
                 (when errorp
                   (format stream "~@<Error ~A: ~A~:@>" error-code
                           (strerror error-code)))
                 (when control-string
                   (apply #'format stream
                          (concatenate 'string (if errorp "~%" "")
                                       "~@<" control-string "~:@>")
                          format-args))))))
  (:documentation "The base class of all LMDB conditions. Conditions
  that are LMDB-SERIOUS-CONDITIONs, but not LMDB-ERRORs are corruption
  and internal errors, which are hard to recover from."))

(define-condition lmdb-error (lmdb-serious-condition error)
  ()
  (:documentation "Base class for normal, recoverable LMDB errors."))

(defun lmdb-error (error-code &optional control-string &rest format-args)
  (with-interrupts
    (error (cond ((eql error-code liblmdb:+keyexist+)
                  'lmdb-key-exists-error)
                 ((eql error-code liblmdb:+notfound+)
                  'lmdb-not-found-error)
                 ((eql error-code liblmdb:+page-notfound+)
                  'lmdb-page-not-found-error)
                 ((eql error-code liblmdb:+corrupted+)
                  'lmdb-corrupted-error)
                 ((eql error-code liblmdb:+panic+)
                  'lmdb-panic-error)
                 ((eql error-code liblmdb:+version-mismatch+)
                  'lmdb-version-mismatch-error)
                 ((eql error-code liblmdb:+invalid+)
                  'lmdb-invalid-error)
                 ((eql error-code liblmdb:+map-full+)
                  'lmdb-map-full-error)
                 ((eql error-code liblmdb:+dbs-full+)
                  'lmdb-dbs-full-error)
                 ((eql error-code liblmdb:+readers-full+)
                  'lmdb-readers-full-error)
                 ((eql error-code liblmdb:+tls-full+)
                  'lmdb-tls-full-error)
                 ((eql error-code liblmdb:+txn-full+)
                  'lmdb-txn-full-error)
                 ((eql error-code liblmdb:+cursor-full+)
                  'lmdb-cursor-full-error)
                 ((eql error-code liblmdb:+page-full+)
                  'lmdb-page-full-error)
                 ((eql error-code liblmdb:+map-resized+)
                  'lmdb-map-resized-error)
                 ((eql error-code liblmdb:+incompatible+)
                  'lmdb-incompatible-error)
                 ((eql error-code liblmdb:+bad-rslot+)
                  'lmdb-bad-rslot-error)
                 ((eql error-code liblmdb:+bad-txn+)
                  'lmdb-bad-txn-error)
                 ((eql error-code liblmdb:+bad-valsize+)
                  'lmdb-bad-valsize-error)
                 ((eql error-code liblmdb:+bad-dbi+)
                  'lmdb-bad-dbi-error)
                 ;; Conditions introduced by this library.
                 ((eql error-code +cursor-uninitialized+)
                  'lmdb-cursor-uninitialized-error)
                 ((eql error-code +cursor-thread+)
                  'lmdb-cursor-thread-error)
                 ((eql error-code +txn-read-only+)
                  'lmdb-txn-read-only-error)
                 ((eql error-code +illegal-access-to-parent-txn+)
                  'lmdb-illegal-access-to-parent-txn-error)
                 (t 'lmdb-error))
           :error-code error-code
           :control-string control-string :format-args format-args)))

(defsection @lmdb-error-code-conditions
    (:title "Conditions for C lmdb error codes")
  "The following conditions correspond to [C lmdb error
  codes](http://www.lmdb.tech/doc/group__errors.html)."
  (lmdb-key-exists-error condition)
  (lmdb-not-found-error condition)
  (lmdb-page-not-found-error condition)
  (lmdb-corrupted-error condition)
  (lmdb-panic-error condition)
  (lmdb-version-mismatch-error condition)
  (lmdb-invalid-error condition)
  (lmdb-map-full-error condition)
  (lmdb-dbs-full-error condition)
  (lmdb-readers-full-error condition)
  (lmdb-txn-full-error condition)
  (lmdb-cursor-full-error condition)
  (lmdb-page-full-error condition)
  (lmdb-map-resized-error condition)
  (lmdb-incompatible-error condition)
  (lmdb-bad-rslot-error condition)
  (lmdb-bad-txn-error condition)
  (lmdb-bad-valsize-error condition)
  (lmdb-bad-dbi-error condition))

(define-condition lmdb-key-exists-error (lmdb-error)
  ()
  (:documentation "Key-value pair already exists. Signalled by PUT
  and CURSOR-PUT."))

(define-condition lmdb-not-found-error (lmdb-error)
  ()
  (:documentation "Key-value pair does not exist. All functions (G3T,
  CURSOR-NEXT, ...) should return NIL instead of signalling this
  error. If it is signalled, that's a bug."))

(define-condition lmdb-page-not-found-error (lmdb-serious-condition)
  ()
  (:documentation "Requested page not found - this usually indicates
  corruption."))

(define-condition lmdb-corrupted-error (lmdb-serious-condition)
  ()
  (:documentation "Located page was wrong type."))

(define-condition lmdb-panic-error (lmdb-serious-condition)
  ()
  (:documentation "Update of meta page failed or environment had fatal
  error."))

(define-condition lmdb-version-mismatch-error (lmdb-error)
  ()
  (:documentation "Environment version mismatch."))

(define-condition lmdb-invalid-error (lmdb-serious-condition)
  ()
  (:documentation "File is not a valid LMDB file."))

(define-condition lmdb-map-full-error (lmdb-error)
  ()
  (:documentation "ENV-MAP-SIZE reached. Reopen the environment with a
  larger :MAP-SIZE."))

(define-condition lmdb-dbs-full-error (lmdb-error)
  ()
  (:documentation "ENV-MAX-DBS reached. Reopen the environment with a
  higher :MAX-DBS."))

(define-condition lmdb-readers-full-error (lmdb-error)
  ()
  (:documentation "ENV-MAX-READERS reached. Reopen the environment
  with a higher :MAX-READERS."))

(define-condition lmdb-txn-full-error (lmdb-error)
  ()
  (:documentation "TXN has too many dirty pages. This condition is
  expected to occur only when using nested read-write transactions or
  operations multiple items (currently not supported by this
  wrapper)."))

(define-condition lmdb-cursor-full-error (lmdb-serious-condition)
  ()
  (:documentation "Cursor stack too deep - internal error."))

(define-condition lmdb-page-full-error (lmdb-serious-condition)
  ()
  (:documentation "Page has not enough space - internal error."))

(define-condition lmdb-map-resized-error (lmdb-error)
  ()
  (:documentation "Data file contents grew beyond ENV-MAP-SIZE. This
  can happen if another OS process using the same environment path set
  a larger map size than this process did."))

(define-condition lmdb-incompatible-error (lmdb-error)
  ()
  (:documentation "Operation and DB incompatible, or DB type changed.
  This can mean:

  - The operation expects a @DUPSORT or DUPFIXED database.

  - Opening a named DB when the unnamed DB has DUPSORT or INTEGER-KEY.

  - Accessing a data record as a database, or vice versa.

  - The database was dropped and recreated with different flags."))

(define-condition lmdb-bad-rslot-error (lmdb-error)
  ()
  (:documentation "Invalid reuse of reader locktable slot. May be
  signalled by WITH-TXN."))

(define-condition lmdb-bad-txn-error (lmdb-error)
  ()
  (:documentation "Transaction must abort, has a child, or is invalid.
  Signalled, for example, when a read-only transaction is nested in a
  read-write transaction, or when a cursor is used whose transaction
  has been closed (committed, aborted, or reset)."))

(define-condition lmdb-bad-valsize-error (lmdb-error)
  ()
  (:documentation "Unsupported size of key/DB name/data, or wrong
  DUPFIXED, INTEGER-KEY or INTEGER-DUP. See ENV-MAX-KEY-SIZE."))

(define-condition lmdb-bad-dbi-error (lmdb-error)
  ()
  (:documentation "The specified DBI was changed unexpectedly."))

(defsection @additional-conditions (:title "Additional conditions")
  "The following conditions do not have a dedicated C lmdb error
  code."
  (lmdb-cursor-uninitialized-error condition)
  (lmdb-cursor-thread-error condition)
  (lmdb-txn-read-only-error condition)
  (lmdb-illegal-access-to-parent-txn-error condition))

(define-condition lmdb-cursor-uninitialized-error (lmdb-error)
  ()
  (:documentation "Cursor was not initialized. Position the cursor at
  a key-value pair with a function like CURSOR-FIRST or
  CURSOR-SET-KEY. Signalled when some functions return the C error
  code `EINVAL`."))

(define-condition lmdb-cursor-thread-error (lmdb-error)
  ()
  (:documentation "Cursor was accessed from a thread other than the
  one in which it was created. Since the foreign cursor object's
  lifetime is tied to the dynamic extent of its WITH-CURSOR, this
  might mean accessing garbage in foreign memory with unpredictable
  consequences."))

(define-condition lmdb-txn-read-only-error (lmdb-error)
  ()
  (:documentation "Attempt was made to write in a read-only
  transaction. Signalled when some functions return the C error code
  `EACCESS`."))

(define-condition lmdb-illegal-access-to-parent-txn-error (lmdb-error)
  ()
  (:documentation """A parent transaction and its cursors may not
  issue any other operations than COMMIT-TXN and ABORT-TXN while it
  has active child transactions. In LMDB, @BASIC-OPERATIONS are always
  executed in the @ACTIVE-TRANSACTION, but @CURSORS can refer to the
  parent transaction:

  ```
  (with-temporary-env (*env*)
    (let ((db (get-db "db" :if-does-not-exist :create)))
      (with-txn (:write t)
        (put db #(1) #(1))
        (with-cursor (cursor db)
          (with-txn (:write t)
            (assert-error lmdb-illegal-access-to-parent-txn-error
              (cursor-set-key #(1) cursor)))))))
  ```"""))


(defsection @version (:title "Library versions")
  (lmdb-foreign-version function)
  (lmdb-binding-version function))

(defun lmdb-foreign-version ()
  "Return the version of the C lmdb library as a string like `0.9.26`.

  Wraps [mdb_version()](http://www.lmdb.tech/doc/group__mdb.html#ga0e5d7298fc39b3c187fffbe30264c968)."
  (without-interrupts
    (cffi:with-foreign-objects ((major :int) (minor :int) (patch :int))
      (liblmdb:version major minor patch)
      (format nil "~D.~D.~D" (cffi:mem-ref major :int)
              (cffi:mem-ref minor :int) (cffi:mem-ref patch :int)))))

(defun lmdb-binding-version ()
  "Return a string representing the version of C lmdb based on which
  the CFFI bindings were created. The version string has the same
  format as LMDB-FOREIGN-VERSION."
  (format nil "~D.~D.~D"
          liblmdb:+version-major+
          liblmdb:+version-minor+
          liblmdb:+version-patch+))


(defsection @environments (:title "Environments")
  "An environment (class ENV) is basically a single memory-mapped file
  holding all the data, plus some flags determining how we interact
  it. An environment can have multiple databases (class DB), each of
  which is a B+ tree within the same file. An environment is like a
  database in a relational db, and the databases in it are like tables
  and indices. The terminology comes from [Berkeley
  DB](https://docs.oracle.com/cd/E17276_01/html/programmer_reference/env.html)."
  (@env-reference section)
  (@opening-and-closing-env section)
  (@misc-env section))

(defsection @env-reference (:title "Environments reference")
  (env class)
  (env-path (reader env))
  (env-max-dbs (reader env))
  (env-max-readers (reader env))
  (env-map-size (reader env))
  (env-mode (reader env))
  (env-flags (reader env)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass env ()
    (;; MDB_env * or NIL if the environment is not open.
     (%envp :initform nil)
     (path
      :initarg :path :reader env-path
      :documentation "The location of the memory-mapped file and the
      environment lock file.")
     (max-dbs
      :type integer :initarg :max-dbs :reader env-max-dbs
      :documentation "The maximum number of named databases in the
      environment. Currently a moderate number is cheap, but a huge
      number gets expensive: 7-120 words per transaction, and every
      GET-DB does a linear search of the opened database.")
     (max-readers
      :type integer :initarg :max-readers :reader env-max-readers
      :documentation "The maximum number of threads/reader slots. See
      the documentation of the [reader lock
      table](http://lmdb.tech/doc/group__readers.html) for more.")
     (map-size
      :type integer :initarg :map-size :reader env-map-size
      :documentation "Specifies the size of the data file in bytes.")
     ;; FIXED-MAP is persisted so ENV-FLAGS may lie about it.
     ;; mdb_dbi_flags() would help.
     (flags
      :type list :initarg :flags :reader env-flags
      :documentation "A plist of the options as captured by OPEN-ENV.
      For example, `(:FIXED-MAP NIL :SUBDIR T ...)`.")
     (mode :initarg :mode :reader env-mode)
     (db-lock :initform (bt:make-lock "db-lock") :reader env-db-lock)
     #-sbcl
     (n-txns-lock :initform nil :initarg :n-txns-lock :reader env-n-txns-lock)
     (n-txns :initform nil :initarg :n-txns :reader env-n-txns))
    (:documentation "An environment object through which a memory-mapped
     data file can be accessed. Always to be created by OPEN-ENV.")))

(declaim (inline %envp))
(defun %envp (env)
  (slot-value env '%envp))

(defsection @opening-and-closing-env
    (:title "Opening and closing environments")
  (open-env function)
  (close-env function)
  (*env* variable)
  (with-env macro)
  (open-env-p function))

(defun %make-env (path &key max-dbs max-readers map-size mode
                  subdir sync meta-sync read-only tls
                  read-ahead lock mem-init fixed-map
                  write-map map-async)
  (assert (not write-map) () "WRITE-MAP not implemented.")
  (assert (not map-async) () "MAP-ASYNC not implemented.")
  (let ((path (pathname path)))
    (if subdir
        (unless (uiop:directory-pathname-p path)
          (lmdb-error nil "Path to environment ~S is not a directory, but ~
                          SUBDIR is true." path))
        (unless (not (uiop:directory-pathname-p path))
          (lmdb-error nil "Path to environment ~S is a directory, but ~
                          SUBDIR is NIL." path)))
    (make-instance 'env :path path :max-dbs max-dbs
                   :max-readers max-readers :map-size map-size :mode mode
                   :flags (list :subdir subdir :sync sync :meta-sync meta-sync
                                :read-only read-only :tls tls
                                :read-ahead read-ahead :lock lock
                                :mem-init mem-init :fixed-map fixed-map
                                :write-map write-map :map-async map-async))))

(defun open-env-p (env)
  "See if ENV is open, i.e. OPEN-ENV has been called on it without a
  corresponding CLOSE-ENV."
  (not (null (%envp env))))

(defmethod print-object ((env env) stream)
  (let ((*print-pretty* nil))
    (print-unreadable-object (env stream :identity t :type t)
      (format stream "~A ~A" (ignore-errors (env-path env))
              (if (open-env-p env) "OPEN" "CLOSED")))))

;;; KLUDGE: We don't want to lose the reference to ENV if
;;; CHECK-FOR-STALE-READERS fails.
(defun check-for-stale-readers* (env)
  (handler-case
      ;; FIXME: This may need a lock for safety according to James.
      (check-for-stale-readers env)
    (lmdb-error (e)
      (warn "CHECK-FOR-STALE-READERS failed with ~S." e))))

(defun env-flag-list-to-int (list)
  (logior (if (getf list :tls) 0 liblmdb:+notls+)
          (if (getf list :fixed-map) liblmdb:+fixedmap+ 0)
          (if (getf list :subdir) 0 liblmdb:+nosubdir+)
          (if (getf list :sync) 0 liblmdb:+nosync+)
          (if (getf list :meta-sync) 0 liblmdb:+nometasync+)
          (if (getf list :read-only) liblmdb:+rdonly+ 0)
          (if (getf list :lock) 0 liblmdb:+nolock+)
          (if (getf list :read-ahead) 0 liblmdb:+nordahead+)
          (if (getf list :mem-init) 0 liblmdb:+nomeminit+)))

;;; MDB-TRUENAME to ENV map, to protect against opening the
;;; same mdb file multiple times.
(defvar *open-envs* (tg:make-weak-hash-table :weakness :value))
(defvar *open-envs-lock* (bt:make-lock "open-envs-lock"))

;;; REGISTER-OPEN-ENV, DEREGISTER-OPEN-ENV and
;;; CHECK-MDB-FILE-NOT-OPEN-IN-SAME-PROCESS are to be called under
;;; WITHOUT-INTERRUPTS and holding *OPEN-ENVS-LOCK*.
(defun register-open-env (env)
  (let ((truename (mdb-truename env)))
    (unless truename
      (lmdb-error nil "Cannot determine TRUENAME of ~S." (env-path env)))
    (when (gethash truename *open-envs*)
      (lmdb-error nil "Another ENV already holds ~S open." truename))
    (setf (gethash truename *open-envs*) env)))

(defun deregister-open-env (env)
  (let ((truename (mdb-truename env)))
    (assert (eq env (gethash truename *open-envs*)))
    (remhash truename *open-envs*)))

(defun check-mdb-file-not-open-in-same-process (env)
  (let ((truename (mdb-truename env)))
    (when (and truename (gethash truename *open-envs*))
      (lmdb-error nil "mdb file ~S already open." truename))))

(defun mdb-truename (env)
  (with-slots (path flags mode) env
    (let ((path (if (getf flags :subdir)
                    (merge-pathnames "data.mdb" path)
                    path)))
      (ignore-errors (truename path)))))

(defun open-env (path &key (if-does-not-exist :error) (synchronized t)
                 (max-dbs 1) (max-readers 126) (map-size (* 1024 1024))
                 (mode #o664)
                 ;; Supported options
                 (subdir t) (sync t) (meta-sync t) read-only (tls t)
                 (read-ahead t) (lock t) (mem-init t) fixed-map
                 ;; Currently unsupported options, must be NIL.
                 write-map map-async)
  "Create an ENV object through which the LMDB environment can be
  accessed and open it. To prevent corruption, an error is signalled
  if the same data file is opened multiple times. However, the checks
  performed do not work on remote filesystems (see ENV-PATH).

  LMDB-ERROR is signalled if opening the environment fails for any
  other reason.

  Unless explicitly noted, none of the arguments persist (i.e. they
  are not saved in the data file).

  PATH is the filesystem location of the environment files (see SUBDIR
  below for more). Do not use LMDB data files on remote filesystems,
  even between processes on the same host. This breaks `flock()` on
  some OSes, possibly memory map sync, and certainly sync between
  programs on different hosts.

  IF-DOES-NOT-EXIST determines what happens if ENV-PATH does not
  exists:

  - :ERROR: An error is signalled.

  - :CREATE: A new memory-mapped file is created ensuring that all
    containing directories exist.

  - `NIL`: Return NIL without doing anything.

  See CLOSE-ENV for the description of SYNCHRONIZED.

  - MAX-DBS: The maximum number of named databases in the environment.
    Currently a moderate number is cheap, but a huge number gets
    expensive: 7-120 words per transaction, and every GET-DB does a
    linear search of the opened database.

  - MAP-SIZE: Specifies the size of the data file in bytes. The new
    size takes effect immediately for the current process, but will
    not be persisted to any others until a write transaction has been
    committed by the current process. Also, only map size increases
    are persisted into the environment. If the map size is increased
    by another process, and data has grown beyond the range of the
    current mapsize, startin a new transaction (see WITH-TXN) will
    signal LMDB-MAP-RESIZED-ERROR. If zero is specified for MAP-SIZE,
    then the persisted size is used from the data file. Also see
    LMDB-MAP-FULL-ERROR.

  - MODE: Unix file mode for files created. The default is `#o664`.
    Has no effect when opening an existing environment.

  The rest of the arguments correspond to LMDB environment flags and
  are available in the plist ENV-FLAGS.

  - SUBDIR: If SUBDIR, then the path is a directory which holds the
    `data.mdb` and the `lock.mdb` files. If SUBDIR is NIL, the path
    is the filename of the data file and the lock file has the same
    name plus a `-lock` suffix.

  - SYNC: If NIL, don't `fsync` after commit. This optimization means
    a system crash can corrupt the database or lose the last
    transactions if buffers are not yet flushed to disk. The risk is
    governed by how often the system flushes dirty buffers to disk and
    how often SYNC-ENV is called. However, if the filesystem preserves
    write order (very few do) and the WRITE-MAP (currently
    unsupported) flag is not used, transactions exhibit
    ACI (atomicity, consistency, isolation) properties and only lose
    D (durability). I.e. database integrity is maintained, but a
    system crash may undo the final transactions.

  - META-SYNC: If NIL, flush system buffers to disk only once per
    transaction, but omit the metadata flush. Defer that until the
    system flushes files to disk, the next commit of a non-read-only
    transaction or SYNC-ENV. This optimization maintains database
    integrity, but a system crash may undo the last committed
    transaction. I.e. it preserves the ACI (atomicity, consistency,
    isolation) but not D (durability) database property.

  - READ-ONLY: Map the data file in read-only mode. It is an error to
    try to modify anything in it.

  - TLS: Setting it to NIL allows each OS thread to have multiple
    read-only transactions (see WITH-TXN's IGNORE-PARENT argument). It
    also allows and transactions not to be tied to a single thread,
    but that's quite dangerous, see @SAFETY.

  - READ-AHEAD: Turn off readahead as in `madvise(MADV_RANDOM)`. Most
    operating systems perform read-ahead on read requests by default.
    This option turns it off if the OS supports it. Turning it off may
    help random read performance when the DB is larger than RAM and
    system RAM is full. This option is not implemented on Windows.

  - LOCK: Data corruption lurks here. If NIL, don't do any locking. If
    concurrent access is anticipated, the caller must manage all
    concurrency itself. For proper operation the caller must enforce
    single-writer semantics, and must ensure that no readers are using
    old transactions while a writer is active. The simplest approach
    is to use an exclusive lock so that no readers may be active at
    all when a writer begins.

  - MEM-INIT: If NIL, don't initialize `malloc`ed memory before
    writing to unused spaces in the data file. By default, memory for
    pages written to the data file is obtained using `malloc`. While
    these pages may be reused in subsequent transactions, freshly
    `malloc`ed pages will be initialized to zeroes before use. This
    avoids persisting leftover data from other code (that used the
    heap and subsequently freed the memory) into the data file. Note
    that many other system libraries may allocate and free memory from
    the heap for arbitrary uses. E.g., stdio may use the heap for file
    I/O buffers. This initialization step has a modest performance
    cost so some applications may want to disable it using this flag.
    This option can be a problem for applications which handle
    sensitive data like passwords, and it makes memory checkers like
    Valgrind noisy. This flag is not needed with WRITE-MAP, which
    writes directly to the mmap instead of using malloc for pages.

  - FIXED-MAP (experimental): This flag must be specified when
    creating the environment and is stored persistently in the data
    file. If successful, the memory map will always reside at the same
    virtual address and pointers used to reference data items in the
    database will be constant across multiple invocations. This option
    may not always work, depending on how the operating system has
    allocated memory to shared libraries and other uses.

  Unsupported flags (an error is signalled if they are changed from
  their default values):

  - WRITE-MAP: Use a writable memory map unless READ-ONLY is set. This
    is faster and uses fewer mallocs, but loses protection from
    application bugs like wild pointer writes and other bad updates
    into the database. Incompatible with nested transactions. This may
    be slightly faster for DBs that fit entirely in RAM, but is slower
    for DBs larger than RAM. Do not mix processes with and without
    WRITE-MAP on the same environment. This can defeat
    durability (SYNC-ENV, etc).

  - MAP-ASYNC: When using WRITE-MAP, use asynchronous flushes to disk.
    As with SYNC NIL, a system crash can then corrupt the database or
    lose the last transactions. Calling #sync ensures on-disk database
    integrity until next commit.

  Open environments have a finalizer attached to them that takes care
  of freeing foreign resources. Thus, the common idiom:

  ```
  (setq *env* (open-env \"some-path\"))
  ```

  is okay for development, too. No need to always do WITH-ENV,
  which does not mesh with threads anyway.

  Wraps [mdb_env_create()](http://www.lmdb.tech/doc/group__mdb.html#gaad6be3d8dcd4ea01f8df436f41d158d4)
  and [mdb_env_open()](http://www.lmdb.tech/doc/group__mdb.html#ga32a193c6bf4d7d5c5d579e71f22e9340)."
  (let ((env (%make-env path :max-dbs max-dbs :max-readers max-readers
                        :map-size map-size :mode mode :subdir subdir
                        :sync sync :meta-sync meta-sync
                        :read-only read-only :tls tls
                        :read-ahead read-ahead :lock lock :mem-init mem-init
                        :fixed-map fixed-map :write-map write-map
                        :map-async map-async))
        (env-created nil)
        (env-registered nil))
    (bt:with-lock-held (*open-envs-lock*)
      (check-mdb-file-not-open-in-same-process env)
      (with-slots (path flags mode) env
        (unless (probe-file path)
          (ecase if-does-not-exist
            (:error (lmdb-error nil "Environment path ~S does not exist."
                                path))
            (:create (ensure-directories-exist path))
            ((nil) (return-from open-env nil))))
        (without-interrupts
          (cffi:with-foreign-object (%envpp :pointer)
            (unwind-protect
                 (let ((return-code (liblmdb:env-create %envpp)))
                   (case return-code
                     (0 (let ((%envp (cffi:mem-ref %envpp :pointer)))
                          (setq env-created t)
                          (assert (zerop (liblmdb:env-set-maxdbs
                                          %envp (env-max-dbs env))))
                          (assert (zerop (liblmdb:env-set-mapsize
                                          %envp (env-map-size env))))
                          (assert (zerop (liblmdb:env-set-maxreaders
                                          %envp (env-max-readers env))))
                          (let ((return-code (liblmdb:env-open
                                              %envp (namestring path)
                                              (env-flag-list-to-int flags)
                                              ;; KLUDGE: For some reason the
                                              ;; mode_t argument becomes a
                                              ;; pointer in the cffi
                                              ;; bindings.
                                              (cffi:make-pointer mode))))
                            (alexandria:switch (return-code)
                              (0
                               (setf (slot-value env '%envp) %envp)
                               (register-open-env env)
                               (setq env-registered t)
                               (tg:finalize env #'(lambda ()
                                                    (finalize-env %envp)))
                               nil)
                              (liblmdb:+version-mismatch+
                               (lmdb-error return-code
                                           "This client does not support the ~
                                           on-disk format version found in ~
                                           the environment files."))
                              (+enoent+
                               (lmdb-error return-code
                                           "The environment path ~S doesn't ~
                                           exist."
                                           path))
                              (+eacces+
                               (lmdb-error +eacces+
                                           "The user doesn't have permission ~
                                           to access the environment path ~S."
                                           path))
                              (liblmdb:+invalid+ (lmdb-error return-code))
                              (+eagain+
                               (lmdb-error +eagain+
                                           "The environment was locked by ~
                                         another process."))
                              (t (lmdb-error return-code))))))
                     (t (lmdb-error return-code
                                    "Error creating environment."))))
              (when (and env-created (not env-registered))
                (let ((%envp (cffi:mem-ref %envpp :pointer)))
                  ;; lmdb.h says that if mdb_env_open fails,
                  ;; mdb_env_close must be called to free the handle
                  ;; allocated by mdb_env_create. Here, it could be that
                  ;; registration failed, too.
                  (liblmdb:env-close %envp)))))))
      (cond (synchronized
             #-sbcl
             (setf (slot-value env 'n-txns-lock) (bt:make-lock))
             (setf (slot-value env 'n-txns) 0))
            (t
             #-sbcl
             (setf (slot-value env 'n-txns-lock) nil)
             (setf (slot-value env 'n-txns) nil))))
    (check-for-stale-readers* env)
    env))

;;; This is only called on open environments because CLOSE-ENV cancels
;;; the finalizer.
(defun finalize-env (%envp)
  (without-interrupts
    (liblmdb:env-close %envp)))

(defun close-env (env &key force)
  "Close ENV and free the memory. Closing an already closed ENV has no effect.

  Since accessing @TRANSACTIONS, @DATABASES and @CURSORS after closing
  their environment would risk database curruption, CLOSE-ENV makes
  sure that they are not in use. There are two ways this can happen:

  - If ENV was opened :SYNCHRONIZED (see OPEN-ENV), then CLOSE-ENV
    waits until there are no @ACTIVE-TRANSACTIONs in ENV before
    closing it. This requires synchronization and introduces some
    overhead, which might be noticable for workloads involving lots of
    quick read transactions. It is an LMDB-ERROR to attempt to close
    an environment in a WITH-TXN to avoid deadlocks.

  - On the other hand, if SYNCHRONIZED was NIL, then - unless FORCE is
    true - calling CLOSE-ENV signals an LMDB-ERROR to avoid the
    @SAFETY issues involved in closing the environment. Environments
    opened with :SYNCHRONIZED NIL are only closed when they are
    garbage collected and their finalizer is run. Still, for
    production it might be worth it to gain the last bit of
    performance.

  Wraps [mdb_env_close()](http://www.lmdb.tech/doc/group__mdb.html#ga4366c43ada8874588b6a62fbda2d1e95)."
  (when (%envp env)
    (when (and (null (env-n-txns env)) (not force))
      (lmdb-error nil "ENVs opened with :SYNCHRONIZED NIL cannot be ~
                    closed explicitly."))
    (check-no-active-transaction env
                                 "Cannot close environment within a WITH-TXN.")
    (without-interrupts
      (when (env-n-txns env)
        (env-prevent-txns env))
      (bt:with-lock-held (*open-envs-lock*)
        (let ((%envp (%envp env)))
          (when %envp
            ;; void in C
            (liblmdb:env-close %envp)
            (setf (slot-value env '%envp) nil)
            (deregister-open-env env)
            (tg:cancel-finalization env)))))))

(defvar *env* nil
  "The default ENV for macros and function that take an environment
  argument.")

(defmacro with-env ((env path &rest open-env-args) &body body)
  """Bind the variable ENV to a new enviroment returned by OPEN-ENV
  called with PATH and OPEN-ENV-ARGS, execute BODY, and CLOSE-ENV. The
  following example binds the default environment:

  ```
  (with-env (*env* "/tmp/lmdb-test" :if-does-not-exist :create)
    ...)
  ```
  """
  `(let ((,env (open-env ,path ,@open-env-args)))
     (unwind-protect
          (progn ,@body)
       (close-env ,env))))

(defsection @misc-env (:title "Miscellaneous environment functions")
  (check-for-stale-readers function)
  (env-statistics function)
  (env-info function)
  (sync-env function)
  (env-max-key-size function)
  (with-temporary-env macro))

(defun check-for-stale-readers (&optional (env *env*))
  "Check for stale entries in the reader lock table. See
  [Caveats](http://www.lmdb.tech/doc/). This function is called
  automatically by OPEN-ENV. If other OS processes or threads
  accessing ENV abort without closing read transactions, call this
  function periodically to get rid off them. Alternatively, close all
  environments accessing the data file.

  Wraps [mdb_reader_check()](http://www.lmdb.tech/doc/group__mdb.html#ga366923d08bb384b3d9580a98edf5d668)."
  (without-interrupts
    (cffi:with-foreign-object (%count :uint32)
      (let ((return-code (liblmdb:reader-check (%envp env) %count)))
        (case return-code
          (0 (cffi:mem-ref %count :uint32))
          (t (lmdb-error return-code)))))))

(defun env-statistics (&optional (env *env*))
  "Return statistics about ENV as a plist.

   - :PAGE-SIZE: The size of a database page in bytes.

   - :DEPTH: The height of the B-tree.

   - :BRANCH-PAGES: The number of internal (non-leaf) pages.

   - :LEAF-PAGES: The number of leaf pages.

   - :OVERFLOW-PAGES: The number of overflow pages.

   - :ENTRIES: The number of data items.

  Wraps [mdb_env_stat()](http://www.lmdb.tech/doc/group__mdb.html#gaf881dca452050efbd434cd16e4bae255)."
  (without-interrupts
    (cffi:with-foreign-object (stat '(:struct liblmdb:stat))
      (liblmdb:env-stat (%envp env) stat)
      (macrolet ((slot (slot)
                   `(cffi:foreign-slot-value stat
                                             '(:struct liblmdb:stat)
                                             ',slot)))
        (list :page-size (slot liblmdb:ms-psize)
              :depth (slot liblmdb:ms-depth)
              :branch-pages (slot liblmdb:ms-branch-pages)
              :leaf-pages (slot liblmdb:ms-leaf-pages)
              :overflow-pages (slot liblmdb:ms-overflow-pages)
              :entries (slot liblmdb:ms-entries))))))

(defun env-info (&optional (env *env*))
  "Return information about ENV as a plist.

  - :MAP-ADDRESS: Address of memory map, if fixed (see OPEN-ENV's
    FIXED-MAP).

  - :MAP-SIZE: Size of the memory map in bytes.

  - :LAST-PAGE-NUMBER: Id of the last used page.

  - :LAST-TXN-ID: Id of the last committed transaction.

  - :MAXIMUM-READERS: The number of reader slots.

  - :N-READERS: The number of reader slots current used.

  Wraps [mdb_env_info()](http://www.lmdb.tech/doc/group__mdb.html#ga18769362c7e7d6cf91889a028a5c5947)."
  (without-interrupts
    (cffi:with-foreign-object (info '(:struct liblmdb:envinfo))
      (liblmdb:env-info (%envp env) info)
      (macrolet ((slot (slot)
                   `(cffi:foreign-slot-value info
                                             '(:struct liblmdb:envinfo)
                                             ',slot)))
        (list :map-address (slot liblmdb:me-mapaddr)
              :map-size (slot liblmdb:me-mapsize)
              :last-page-number (slot liblmdb:me-last-pgno)
              :last-txn-id (slot liblmdb:me-last-txnid)
              :maximum-readers (slot liblmdb:me-maxreaders)
              :n-readers (slot liblmdb:me-numreaders))))))

(defun sync-env (&optional (env *env*))
  "Flush the data buffers to disk as in calling `fsync()`. When ENV
  had been opened with :SYNC NIL or :META-SYNC NIL, this may be handy
  to force flushing the OS buffers to disk, which avoids potential
  durability and integrity issues.

  Wraps [mdb_env_sync()](http://www.lmdb.tech/doc/group__mdb.html#ga85e61f05aa68b520cc6c3b981dba5037)."
  (without-interrupts
    (liblmdb:env-sync (%envp env) 1)))

(defun env-max-key-size (&optional (env *env*))
  "Return the maximum size of keys and @DUPSORT data in bytes. Depends
  on the compile-time constant `MDB_MAXKEYSIZE` in the C library. The
  default is 511. If this limit is exceeded LMDB-BAD-VALSIZE-ERROR is
  signalled.

  Wraps [mdb_env_get_maxkeysize()](http://www.lmdb.tech/doc/group__mdb.html#gaaf0be004f33828bf2fb09d77eb3cef94)."
  (without-interrupts
    (liblmdb:env-get-maxkeysize (%envp env))))

(defmacro with-temporary-env ((env &rest open-env-args) &body body)
  """Run BODY with an open temporary environment bound to ENV. In more
  detail, create an environment in a fresh temporary directory in an
  OS specific location. OPEN-ENV-ARGS is a list of keyword arguments
  and values for OPEN-ENV. This macro is intended for testing and
  examples.

  ```
  (with-temporary-env (*env*)
    (let ((db (get-db "test" :if-does-not-exist :create)))
      (with-txn (:write t)
        (put db "k1" #(2 3))
        (print (g3t db "k1")) ; => #(2 3)
        (del db "k1"))))
  ```

  Since data corruption in temporary environments is not a concern,
  unlike WITH-ENV, WITH-TEMPORARY-ENV closes the environment even if
  it was opened with
  :SYNCHRONIZED NIL (see OPEN-ENV and CLOSE-ENV)."""
  (alexandria:with-gensyms (path)
    `(call-with-temporary-env
      (lambda (,path)
        (with-env (,env ,path :if-does-not-exist :create ,@open-env-args)
          (unwind-protect
               (progn ,@body)
            (unless ,(getf open-env-args :synchronized t)
              (close-env ,env :force t)))))
      ,@open-env-args)))

(defun random-string ()
  (format nil "~:@(~36,8,'0R~)" (random (expt 36 8) (make-random-state t))))

(defun call-with-temporary-env (fn &rest env-args)
  (let* ((temp-dir (merge-pathnames
                    (make-pathname :directory (list :relative (random-string)))
                    (uiop:temporary-directory)))
         (subdir (getf env-args :subdir t))
         (path (if subdir
                   temp-dir
                   (merge-pathnames (random-string) temp-dir))))
    (assert (not (uiop:directory-exists-p temp-dir)))
    (ensure-directories-exist path)
    (unwind-protect
         (funcall fn path)
      (uiop:delete-directory-tree temp-dir :validate t
                                  :if-does-not-exist :ignore))))


(defsection @transactions (:title "Transactions")
  "The LMDB environment supports transactional reads and writes. By
  default, these provide the standard ACID (atomicity, consistency,
  isolation, durability) guarantees. Writes from a transaction are not
  immediately visible to other transactions. When the transaction is
  committed, all its writes become visible atomically for future
  transactions even if Lisp crashes or there is power failure. If the
  transaction is aborted, its writes are discarded.

  Transactions span the entire environment (see ENV). All the updates
  made in the course of an update transaction - writing records across
  all databases, creating databases, and destroying databases - are
  either completed atomically or rolled back.

  Write transactions can be nested. Child transactions see the
  uncommitted writes of their parent. The child transaction can commit
  or abort, at which point its writes become visible to the parent
  transaction or are discarded. If the parent aborts, all of the
  writes performed in the context of the parent, including those from
  committed child transactions, are discarded."
  (with-txn macro)
  (@active-transaction glossary-term)
  (open-txn-p function)
  (txn-id function)
  (commit-txn function)
  (abort-txn function)
  (renew-txn function)
  (reset-txn function)
  (@nesting-transactions section))

(declaim (inline make-txn txn-%txna txn-flags))
(defstruct txn
  ;; %TXNA is an aligned pointer converted to a fixnum with
  ;; %ALIGNED-POINTER-TO-FIXNUM, possibly with its sign flipped to
  ;; %negative if the transaction is reset.
  (%txna 0 :type fixnum)
  (flags 0 :type fixnum)
  ;; Keep FINALIZE-ENV at bay.
  (env nil :type env)
  ;; This might be a TXN with a different ENV.
  (enclosing-txn nil :type (or txn null)))

(declaim (type (or txn null) *txn*))
(defvar *txn* nil)
#+sbcl
(declaim (sb-ext:always-bound *txn*))

(define-glossary-term @active-transaction (:title "active transaction")
  """The active transaction in some environment and thread is the
  transaction of the innermost WITH-TXN being executed in the thread
  that belongs to the environment. In most cases, this is simply the
  enclosing WITH-TXN, but if WITH-TXNs with different :ENV arguments
  are nested, then it may not be:

  ```
  (with-temporary-env (env)
    (let ((db (get-db "db" :env env :if-does-not-exist :create)))
      (with-temporary-env (inner-env)
        (with-txn (:env env :write t)
          (with-txn (:env inner-env)
            (put db #(1) #(2)))))))
  ```

  In the above example, DB is known to belong to ENV so although the
  immediately enclosing transaction belongs to INNER-ENV, PUT is
  executed in context of the outer, write transaction because that's
  the innermost in ENV.

  Operations that require a transaction always attempt to use the
  active transaction even if it is not open (see OPEN-TXN-P).""")

;;; Return the innermost TXN in ENV.
(declaim (inline active-txn-in-env))
(defun active-txn-in-env (env &optional errorp)
  (let ((txn (if (null env)
                 *txn*
                 (let ((txn *txn*))
                   (loop while txn do
                     (when (eq (txn-env txn) env)
                       (return txn))
                     (setq txn (txn-enclosing-txn txn)))))))
    (if (and errorp (null txn))
        (lmdb-error liblmdb:+bad-txn+)
        txn)))

(defun check-no-active-transaction (env message)
  (when (active-txn-in-env env)
    (lmdb-error nil message)))

;;; Traverse the list of ancestors of TXN through the
;;; TXN-ENCLOSING-TXN and return the first that has the same ENV.
(defun parent-txn (txn)
  (let ((env (txn-env txn))
        (enclosing (txn-enclosing-txn txn)))
    (loop while enclosing do
      (when (eq (txn-env enclosing) env)
        (return-from parent-txn enclosing))
      (setq enclosing (txn-enclosing-txn enclosing)))))

(declaim (inline %txna))
(defun %txna ()
  (txn-%txna *txn*))

(declaim (inline read-only-txn-p))
(defun read-only-txn-p (txn)
  (logtest (txn-flags txn) liblmdb:+rdonly+))

(declaim (inline close-txn))
(defun close-txn (txn)
  (setf (txn-%txna txn) 0))

(defun txn-reset-p (txn)
  (minusp (txn-%txna txn)))

(defun set-txn-reset-p (txn)
  (setf (txn-%txna txn) (- (abs (txn-%txna txn)))))

(defun clear-txn-reset-p (txn)
  (setf (txn-%txna txn) (abs (txn-%txna txn))))

(declaim (inline %open-txn-p))
(defun %open-txn-p (txn)
  (plusp (txn-%txna txn)))

(declaim (inline open-txn-p))
(defun open-txn-p (&optional env)
  "See if there is an active transaction and it is open, i.e.
  COMMIT-TXN or ABORT-TXN have not been called on it. Also, RESET-TXN
  without a corresponding RENEW-TXN closes the transaction."
  (let ((active-txn (active-txn-in-env env)))
    (and active-txn (%open-txn-p active-txn))))

(declaim (inline abortable-txn-p))
(defun abortable-txn-p (txn)
  (not (zerop (txn-%txna txn))))

(declaim (inline %txnp))
(defun %txnp (&optional (%txna (%txna)))
  (fixnum-to-aligned-pointer (abs %txna)))

(defun txn-id ()
  "The ID of TXN. IDs are integers incrementing from 1. For a
  read-only transaction, this corresponds to the snapshot being read;
  concurrent readers will frequently have the same transaction ID.
  Only committed write transactions increment the ID. If a transaction
  aborts, the ID may be re-used by the next writer."
  (liblmdb:txn-id (%txnp)))

(defmacro with-txn ((&key (env '*env*) write ignore-parent
                     (sync t) (meta-sync t))
                    &body body)
  "Start a transaction in ENV, execute BODY. Then, if the transaction
  is open (see OPEN-TXN-P) and BODY returned normally, attempt to
  commit the transaction. Next, if BODY performed a non-local exit or
  committing failed, but the transaction is still open, then abort it.
  It is explicitly allowed to call COMMIT-TXN or ABORT-TXN within
  WITH-TXN.

  Transactions provide ACID guarantees (with SYNC and META-SYNC both
  on). They span the entire environment, they are not specific to
  individual DB.

  - If WRITE is NIL, the transaction is read-only and no writes (e.g.
    PUT) may be performed in the transaction. On the flipside, many
    read-only transactions can run concurrently (see ENV-MAX-READERS),
    while write transactions are mutually exclusive. Furthermore, the
    single write transaction can also run concurrently with read
    transactions, just keep in mind that read transactions hold on to
    the state of the environment at the time of their creation and
    thus prevent pages since replaced from being reused.

  - If IGNORE-PARENT is true, then in an enclosing WITH-TXN, instead
    of creating a child transaction, start an independent transaction.

  - If SYNC is NIL, then no flushing of buffers will take place after
    a commit as if the environment had been opened with :SYNC NIL.

  - Likewise, META-SYNC is the per-transaction equivalent of the
    OPEN-ENV's META-SYNC.

  Also see @NESTING-TRANSACTIONS.

  Wraps [mdb_txn_begin()](http://www.lmdb.tech/doc/group__mdb.html#gad7ea55da06b77513609efebd44b26920)."
  (alexandria:with-gensyms (with-txn-body)
    `(flet ((,with-txn-body ()
              ,@body))
       (declare (dynamic-extent #',with-txn-body))
       (call-with-txn #',with-txn-body ,env ,write ,ignore-parent
                      ,sync ,meta-sync))))

(declaim (inline env-enter-txn))
(defun env-enter-txn (env)
  #-sbcl
  (let ((n-txns-lock (slot-value env 'n-txns-lock))
        (errorp nil))
    (when n-txns-lock
      (bt:with-lock-held (n-txns-lock)
        (with-slots (n-txns) env
          (when (= n-txns -1)
            (setq errorp t))
          (incf (slot-value env 'n-txns))))
      (when errorp
        (lmdb-error nil "Attempt to start a transaction in a ~
                        closed enviroment ~S." env))))
  #+sbcl
  (with-slots (n-txns) env
    (let ((old n-txns))
      (declare (type (or fixnum null) old))
      (when old
        (loop when (eq old -1)
                do (lmdb-error nil "Attempt to start a transaction in a ~
                                   closed enviroment ~S." env)
              until (eq (sb-ext:compare-and-swap n-txns old (1+ old)) old)
              do (setq old n-txns))))))

(declaim (inline env-leave-txn))
(defun env-leave-txn (env)
  #-sbcl
  (let ((n-txns-lock (slot-value env 'n-txns-lock)))
    (when n-txns-lock
      (bt:with-lock-held (n-txns-lock)
        (decf (slot-value env 'n-txns)))))
  #+sbcl
  (with-slots (n-txns) env
    (let ((old n-txns))
      (declare (type (or fixnum null) old))
      (when old
        (loop do (assert (not (eq old -1)))
              until (eq (sb-ext:compare-and-swap n-txns old (1- old)) old)
              do (setq old n-txns))))))

;;; Set ENV-N-TXNS to -1, which makes ENV-ENTER-TXN fail with an error.
(defun env-prevent-txns (env)
  #-sbcl
  (let ((n-txns-lock (slot-value env 'n-txns-lock)))
    (when n-txns-lock
      (loop
        (bt:with-lock-held (n-txns-lock)
          (with-slots (n-txns) env
            (when (zerop n-txns)
              (setq n-txns -1)
              (return)))))))
  #+sbcl
  (with-slots (n-txns) env
    (let ((old n-txns))
      (declare (type (or fixnum null) old))
      (assert old)
      (loop until (eq (sb-ext:compare-and-swap n-txns old -1) old)
            do (setq old n-txns)))))

;;; This is equivalent to (WHEN (%OPEN-TXN-P TXN) (COMMIT-TXN)), but
;;; access to TXN and its slots is optimized.
(declaim (inline maybe-commit-txn))
(defun maybe-commit-txn (txn)
  (declare (type txn txn)
           (optimize speed))
  (let ((%txna (txn-%txna txn)))
    (when (plusp %txna)
      (let ((return-code (liblmdb:txn-commit
                          (fixnum-to-aligned-pointer %txna))))
        (alexandria:switch (return-code)
          (0 (setf (txn-%txna txn) 0))
          (t (lmdb-error return-code)))))))

;;; An optimized equivalent of ABORT-TXN
(declaim (inline maybe-abort-txn))
(defun maybe-abort-txn (txn)
  (declare (type txn txn)
           (optimize speed))
  (let ((%txna (txn-%txna txn)))
    (unless (zerop %txna)
      (liblmdb:txn-abort (fixnum-to-aligned-pointer (abs %txna)))
      (setf (txn-%txna txn) 0))))

(defun call-with-txn (fn env write ignore-parent sync meta-sync)
  (declare (type function fn)
           (type (or env null) env)
           (optimize speed))
  (unless env
    (lmdb-error nil "WITH-TXN: ENV is NIL."))
  (let ((parent-txn (active-txn-in-env env)))
    (when (and (not write) (not ignore-parent)
               parent-txn (%open-txn-p parent-txn))
      (return-from call-with-txn (funcall fn)))
    (when (and write ignore-parent
               parent-txn (%open-txn-p parent-txn)
               (not (read-only-txn-p parent-txn)))
      (lmdb-error liblmdb:+bad-txn+ "Two write transactions in the same ~
                                    thread would deadlock."))
    (without-interrupts
      (env-enter-txn env)
      (let* ((flags (logior (if write 0 liblmdb:+rdonly+)
                            (if sync 0 liblmdb:+nosync+)
                            (if meta-sync 0 liblmdb:+nometasync+)))
             (%txna (cffi:with-foreign-object (%txnpp :pointer)
                      (let ((return-code (liblmdb:txn-begin
                                          (%envp env)
                                          (if (or ignore-parent
                                                  (null parent-txn))
                                              +null-pointer+
                                              (%txnp (txn-%txna parent-txn)))
                                          flags %txnpp)))
                        (unless (zerop return-code)
                          (lmdb-error return-code))
                        (aligned-pointer-to-fixnum
                         (cffi:mem-ref %txnpp :pointer))))))
        (let* ((txn (make-txn :%txna %txna :flags flags :env env
                              :enclosing-txn *txn*))
               (*txn* txn))
          (declare (type txn txn)
                   (dynamic-extent txn))
          (unwind-protect
               (multiple-value-prog1
                   (with-interrupts (funcall fn))
                 (maybe-commit-txn txn))
            (maybe-abort-txn txn)
            (env-leave-txn env)))))))

(declaim (inline open-%txnp))
(defun open-%txnp (txn)
  (if (null txn)
      (lmdb-error liblmdb:+bad-txn+ "No active transaction.")
      (let ((%txna (txn-%txna txn)))
        (unless (plusp %txna)
          (lmdb-error liblmdb:+bad-txn+ "The active transaction is not open."))
        (fixnum-to-aligned-pointer %txna))))

(declaim (inline open-%txnp-for-env))
(defun open-%txnp-for-env (env)
  (let ((active-txn (if (or (null env)
                            (null *txn*)
                            (eq (txn-env *txn*) env))
                        *txn*
                        (active-txn-in-env env))))
    (unless active-txn
      (lmdb-error liblmdb:+bad-txn+ "No active transaction."))
    (let ((%txna (txn-%txna active-txn)))
      (unless (plusp %txna)
        (lmdb-error liblmdb:+bad-txn+ "The active transaction is not open."))
      (fixnum-to-aligned-pointer %txna))))

(declaim (inline open-%txnp-for-db))
(defun open-%txnp-for-db (db)
  (open-%txnp-for-env (slot-value db 'env)))

(defun commit-txn (&optional env)
  "Commit the innermost enclosig transaction (or @ACTIVE-TRANSACTION
  belonging to ENV if ENV is specified) or signal an error if it is
  not open. If TXN is not nested in another transaction, committing
  makes updates performed visible to future transactions. If TXN is a
  child transaction, then committing makes updates visible to its
  parent only. For read-only transactions, committing releases the
  reference to a historical version environment, allowing reuse of
  pages replaced since.

  Wraps [mdb_txn_commit()](http://www.lmdb.tech/doc/group__mdb.html#ga846fbd6f46105617ac9f4d76476f6597)."
  (without-interrupts
    (let ((return-code (liblmdb:txn-commit (open-%txnp-for-env env))))
      (alexandria:switch (return-code)
        (0 (close-txn *txn*)
           nil)
        (t (lmdb-error return-code))))))

(defun abort-txn (&optional env)
  "Close TXN by discarding all updates performed, which will then not
  be visible to either parent or future transactions. Aborting an
  already closed transaction is a noop. Always succeeds.

  Wraps [mdb_txn_abort()](http://www.lmdb.tech/doc/group__mdb.html#ga73a5938ae4c3239ee11efa07eb22b882)."
  (let ((active-txn (active-txn-in-env env)))
    (when (abortable-txn-p active-txn)
      (without-interrupts
        (liblmdb:txn-abort (open-%txnp active-txn))
        (close-txn active-txn)))))

(defun reset-txn (&optional env)
  "Abort the open, read-only TXN, release the reference to the
  historical version of the environment, but make it faster to start
  another read-only transaction with RENEW-TXN. This is accomplished
  by not deallocating some data structures, and keeping the slot in
  the reader table. Cursors opened within the transaction must not be
  used again, except if renewed (see RENEW-CURSOR). If TXN is an open,
  read-only transaction, this function always succeeds.

  Wraps [mdb_txn_reset()](http://www.lmdb.tech/doc/group__mdb.html#ga02b06706f8a66249769503c4e88c56cd)."
  (let ((active-txn (active-txn-in-env env)))
    (unless (read-only-txn-p active-txn)
      (lmdb-error nil "Cannot reset write transaction."))
    (without-interrupts
      (liblmdb:txn-reset (open-%txnp active-txn))
      (set-txn-reset-p active-txn))))

(defun renew-txn (&optional env)
  "Renew TXN that was reset by RESET-TXN. This acquires a new reader
  lock that had been released by RESET-TXN. After renewal, it is as if
  TXN had just been started.

  Wraps [mdb_txn_renew()](http://www.lmdb.tech/doc/group__mdb.html#ga6c6f917959517ede1c504cf7c720ce6d)."
  (let ((active-txn (active-txn-in-env env)))
    (unless (txn-reset-p active-txn)
      (lmdb-error nil "Cannot renew transaction because it was not reset."))
    (without-interrupts
      (let ((return-code (liblmdb:txn-renew
                          (%txnp (txn-%txna active-txn)))))
        (alexandria:switch (return-code)
          (0 (clear-txn-reset-p active-txn)
             nil)
          (t (lmdb-error return-code)))))))

(defsection @nesting-transactions (:title "Nesting transactions")
  """When WITH-TXNs are nested (i.e. one is executed in the dynamic
  extent of another), we speak of nested transactions. Transaction can
  be nested to arbitrary levels. Child transactions may be committed
  or aborted independently from their parent transaction (the
  immediately enclosing WITH-TXN). Committing a child transaction only
  makes the updates made by it visible to the parent. If the parent
  then aborts, the child's updates are aborted too. If the parent
  commits, all child transactions that were not aborted are committed,
  too.

  Actually, the C lmdb library only supports nesting write
  transactions. To simplify usage, the Lisp side turns read-only
  WITH-TXNs nested in another WITH-TXNs into noops.

  ```
  (with-temporary-env (*env*)
    (let ((db (get-db "test" :if-does-not-exist :create
                      :value-encoding :uint64)))
      ;; Create a top-level write transaction.
      (with-txn (:write t)
        (put db "p" 0)
        ;; First child transaction
        (with-txn (:write t)
          ;; Writes of the parent are visible in children.
          (assert (= (g3t db "p") 0))
          (put db "c1" 1))
        ;; Parent sees what the child committed (but it's not visible to
        ;; unrelated transactions).
        (assert (= (g3t db "c1") 1))
        ;; Second child transaction
        (with-txn (:write t)
          ;; Sees writes from the parent that came from the first child.
          (assert (= (g3t db "c1") 1))
          (put db "c1" 2)
          (put db "c2" 2)
          (abort-txn)))
      ;; Create a top-level read transaction to check what was committed.
      (with-txn ()
        ;; Since the second child aborted, its writes are discarded.
        (assert (= (g3t db "p") 0))
        (assert (= (g3t db "c1") 1))
        (assert (null (g3t db "c2"))))))
  ```

  COMMIT-TXN, ABORT-TXN, and RESET-TXN all close the transaction (see
  OPEN-TXN-P), which prevents database operations such as G3T, PUT,
  DEL within that transaction. Furthermore, any @CURSORS created in the
  context of the transaction will no longer be valid (but see
  CURSOR-RENEW).

  An LMDB parent transaction and its cursors must not issue operations
  other than COMMIT-TXN and ABORT-TXN while there are active child
  transactions. As the Lisp side does not expose transaction objects
  directly, performing @BASIC-OPERATIONS in the parent transaction is
  not possible, but it is possible with @CURSORS as they are tied to
  the transaction in which they were created.

  IGNORE-PARENT true overrides the default nesting semantics of
  WITH-TXN and creates a new top-level transaction, which is not a
  child of the enclosing WITH-TXN.

  - Since LMDB is single-writer, on nesting an IGNORE-PARENT write
    transaction in another write transaction, LMDB-BAD-TXN-ERROR is
    signalled to avoid the deadlock.

  - Nesting a read-only WITH-TXN with IGNORE-PARENT in another
    read-only WITH-TXN is LMDB-BAD-RSLOT-ERROR error with the TLS
    option because it would create two read-only transactions in the
    same thread.

  Nesting a read transaction in another transaction would be an
  LMDB-BAD-RSLOT-ERROR according to the C lmdb library, but a
  read-only WITH-TXN with IGNORE-PARENT NIL nested in another WITH-TXN
  is turned into a noop so this edge case is papered over.
  """)


(defsection @databases (:title "Databases")
  (@the-unnamed-database section)
  (@dupsort section)
  (@database-api section))

(defsection @the-unnamed-database (:title "The unnamed database")
  "LMDB has a default, unnamed database backed by a B+ tree. This db
  can hold normal key-value pairs and named databases. The unnamed
  database can be accessed by passing NIL as the database name to
  GET-DB. There are some restrictions on the flags of the unnamed
  database, see LMDB-INCOMPATIBLE-ERROR.")

(defsection @dupsort (:title "DUPSORT")
  "A prominent feature of LMDB is the ability to associate multiple
  sorted values with keys, which is enabled by the DUPSORT argument of
  GET-DB. Just as a named database is a B+ tree associated with a
  key (its name) in the B+ tree of the unnamed database, so do these
  sorted duplicates form a B+ tree under a key in a named or the
  unnamed database. Among the @BASIC-OPERATIONS, PUT and DEL are
  equipped to deal with duplicate values, but G3T is too limited, and
  @CURSORS are needed to make full use of DUPSORT.

  When using this feature the limit on the maximum key size applies to
  duplicate data, as well. See ENV-MAX-KEY-SIZE.")

(defsection @database-api (:title "Database API")
  (get-db function)
  (db class)
  (db-name (reader db))
  (drop-db function)
  (db-statistics function))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass db ()
    (;; This is an unsigned int.
     (dbi :initarg :dbi :reader %dbi)
     (name
      :reader db-name
      :initarg :name
      :type string
      :documentation "The name of the database.")
     (env :initarg :env :reader db-env)
     (key-encoding
      :initarg :key-encoding
      :reader db-key-encoding)
     (value-encoding
      :initarg :value-encoding
      :reader db-value-encoding))
    (:documentation "A database in an environment (class ENV). Always to
     be created by GET-DB.")))

(defmethod print-object ((object db) stream)
  (let ((*print-pretty* nil))
    (print-unreadable-object (object stream :identity t :type t)
      (format stream "~s" (if (slot-boundp object 'name)
                              (slot-value object 'name)
                              "?")))))

(deftype encoding ()
  '(member nil :uint64 :octets :utf-8))

(defun get-db (name &key (env *env*)
               (if-does-not-exist :error)
               key-encoding value-encoding
               integer-key reverse-key
               dupsort integer-dup reverse-dup dupfixed)
  "Open the database with NAME in the open environment ENV, and return
  a DB object. If NAME is NIL, then the @THE-UNNAMED-DATABASE is
  opened. Must not be called from an open transaction. This is because
  GET-DB starts a transaction itself to comply with C lmdb's
  requirements (see @SAFETY).

  If IF-DOES-NOT-EXIST is :CREATE, then a new named database is
  created. If IF-DOES-NOT-EXIST is :ERROR, then an error is signalled
  if the database does not exists.

  KEY-ENCODING and VALUE-ENCODING are both one of NIL, :UINT64,
  :OCTETS or :UTF-8. KEY-ENCODING is set to :UINT64 when INTEGER-KEY
  is true. VALUE-ENCODING is set to :UINT64 when INTEGER-DUP is true.
  Note that changing the encoding does *not* reencode already existing
  data. encoding. See @ENCODINGS for their full semantics.

  The recommended practice is to open a database in a process once.
  This leaves the db open for use in subsequent transactions.

  The following flags are for database creation, they do not have any
  effect in subsequent calls (except for the @THE-UNNAMED-DATABASE).

  - INTEGER-KEY: Keys in the database are C `unsigned` or `size_t`
    integers encoded in native byte order. Keys must all be either
    `unsigned` or `size_t`, they cannot be mixed in a single database.

  - REVERSE-KEY: Keys are strings to be compared in reverse order,
    from the end of the strings to the beginning. By default, keys are
    treated as strings and compared from beginning to end.

  - DUPSORT: Duplicate keys may be used in the database (or, from
    another perspective, keys may have multiple values, stored in
    sorted order). By default, keys must be unique and may have only a
    single value. Also, see @DUPSORT.

  - INTEGER-DUP: This option specifies that duplicate data items are
    binary integers, similarly to INTEGER-KEY. Only matters if
    DUPSORT.

  - REVERSE-DUP: This option specifies that duplicate data items
    should be compared as strings in reverse order. Only matters if
    DUPSORT.

  - DUPFIXED: This flag may only be used in combination DUPSORT. When
    true, data items for this database must all be the same size,
    which allows further optimizations in storage and retrieval.
    Currently, the wrapper functions that could take advantage of
    this (e.g. PUT, CURSOR-PUT, CURSOR-NEXT and CURSOR-VALUE), do not.

  No function to close a database (an equivalent to
  [mdb_dbi_close()](http://www.lmdb.tech/doc/group__mdb.html#ga52dd98d0c542378370cd6b712ff961b5))
  is provided due to subtle races and corruption it could cause when
  an `MDB_dbi` (unsigned integer, similar to an fd) is assigned by a
  subsequent open to another named database.

  Wraps [mdb_dbi_open()](http://www.lmdb.tech/doc/group__mdb.html#gac08cad5b096925642ca359a6d6f0562a)."
  (check-type env env)
  (check-type key-encoding encoding)
  (check-type value-encoding encoding)
  (when integer-key
    (check-type key-encoding (member nil :uint64))
    (setq key-encoding :uint64))
  (when integer-dup
    (check-type value-encoding (member nil :uint64))
    (setq value-encoding :uint64))
  (flet ((%open-db ()
           (cffi:with-foreign-object (%dbip :uint)
             (let* ((flags (logior (ecase if-does-not-exist
                                     (:create liblmdb:+create+)
                                     (:error 0))
                                   (if integer-key liblmdb:+integerkey+ 0)
                                   (if reverse-key liblmdb:+reversekey+ 0)
                                   (if dupsort liblmdb:+dupsort+ 0)
                                   (if integer-dup liblmdb:+integerdup+ 0)
                                   (if reverse-dup liblmdb:+reversedup+ 0)
                                   (if dupfixed liblmdb:+dupfixed+ 0)))
                    (return-code (liblmdb:dbi-open (%txnp)
                                                   (or name +null-pointer+)
                                                   flags %dbip)))
               (alexandria:switch (return-code)
                 (0 (make-instance 'db :dbi (cffi:mem-ref %dbip :uint)
                                   :name name :env env
                                   :key-encoding key-encoding
                                   :value-encoding value-encoding))
                 (liblmdb:+notfound+
                  (ecase if-does-not-exist
                    (:error
                     (lmdb-error
                      return-code
                      "Database ~S not found, and ~
                      :IF-DOES-NOT-EXIST :CREATE was not specified."
                      name))
                    ((nil) nil)))
                 (liblmdb:+dbs-full+
                  (lmdb-error
                   return-code
                   "Reached maximum number of named databases."))
                 (t
                  (lmdb-error return-code)))))))
    (let ((active-txn (active-txn-in-env env)))
      (when (and active-txn (%open-txn-p active-txn))
        (lmdb-error nil "GET-DB must not be called in a open transaction.")))
    (without-interrupts
      ;; "This function must not be called from multiple concurrent
      ;; transactions in the same process. A transaction that uses
      ;; this function must finish (either commit or abort) before any
      ;; other transaction in the process may use this function."
      (bt:with-lock-held ((env-db-lock env))
        (with-txn (:env env :write (eq if-does-not-exist :create))
          (%open-db))))))

(defun drop-db (name path &key open-env-args (delete t))
  "Empty the database with NAME in the environment denoted by PATH. If
  DELETE, then delete the database. Since closing a database is
  dangerous (see GET-DB), DROP-DB opens and closes the environment
  itself.

  Wraps [mdb_drop()](http://www.lmdb.tech/doc/group__mdb.html#gab966fab3840fc54a6571dfb32b00f2db)."
  (without-interrupts
    (let ((env (apply #'open-env path open-env-args)))
      (unwind-protect
           (let ((db (get-db name :env env)))
             (with-txn (:write t :env env :ignore-parent t)
               (liblmdb:drop (%txnp) (%dbi db) (if delete 1 0))))
        (close-env env)))))

(defun db-statistics (db)
  "Return statistics about the database.

  Wraps [mdb_stat()](http://www.lmdb.tech/doc/group__mdb.html#gae6c1069febe94299769dbdd032fadef6)."
  (cffi:with-foreign-object (%stat '(:struct liblmdb:stat))
    (without-interrupts
      (liblmdb:stat (open-%txnp-for-db db) (%dbi db) %stat))
    (macrolet ((slot (slot)
                 `(cffi:foreign-slot-value %stat '(:struct liblmdb:stat)
                                           ',slot)))
      (list :page-size (slot liblmdb:ms-psize)
            :depth (slot liblmdb:ms-depth)
            :branch-pages (slot liblmdb:ms-branch-pages)
            :leaf-pages (slot liblmdb:ms-leaf-pages)
            :overflow-pages (slot liblmdb:ms-overflow-pages)
            :entries (slot liblmdb:ms-entries)))))

(defun db-flags (db)
  (without-interrupts
    (cffi:with-foreign-object (%flagsp :uint)
      (liblmdb:dbi-flags (open-%txnp-for-db db) (%dbi db) %flagsp)
      (cffi:mem-ref %flagsp :uint))))


(defsection @encodings (:title "Encoding and decoding data")
  """In the C lmdb library, keys and values are opaque byte vectors
  only ever inspected internally to maintain the sort order (of keys
  and duplicate values if @DUPSORT). The client is given the freedom
  and the responsibility to choose how to perform conversion to and
  from byte vectors.

  LMDB exposes this full flexibility while at the same time providing
  reasonable defaults for the common cases. In particular, with the
  KEY-ENCODING and VALUE-ENCODING arguments of GET-DB, the
  data (meaning the key or value here) encoding can be declared
  explicitly. The following values are supported:

  - :UINT64: Data to be encoded must be of type '(UNSIGNED-BYTE 64)`,
    which is then encoded as an 8 byte array in _native_ byte order.
    The reverse transformation takes place when returning values. This
    is the encoding used for INTEGER-KEY and INTEGER-DUP DBs.

  - :OCTETS: Note the plural. Data to be encoded (e.g. KEY argument of
    G3T) must be a 1D byte array. If its element type
    is `(UNSIGNED-BYTE 8)`, then the data can be passed to the foreign
    code more efficiently, but declaring the element type is not
    required. For example, VECTORs can be used as long as the actual
    elements are of type `(UNSIGNED-BYTE 8)`. Foreign byte arrays to
    be decoded (e.g. the value returned by G3T) are simply returned as
    a Lisp array.

  - :UTF-8: Data to be encoded must be a string, which is converted to
    octets by TRIVIAL-UTF-8. Null-terminated. Foreign byte arrays are
    decoded the same way.

  - NIL: Data is encoded using the default encoding according to its
    Lisp type: strings as :UTF-8, vectors as :OCTETS, `(UNSIGNED-BYTE
    64)` as :UINT64. Decoding is always performed as :OCTETS.

  Even if the encoding is undeclared, it is recommended to use a
  single type for keys (and duplicate values) to avoid unexpected
  conflicts that could arise, for example, when the UTF-8 encoding of
  a string and the :UINT64 encoding of an integer coincide. The same
  consideration doubly applies to named databases, which share the key
  space with normal key-value pairs in the default database (see
  @THE-UNNAMED-DATABASE).

  Together, :UINT64 and :UTF-8 cover the common cases for keys. They
  trade off dynamic typing for easy sortability (using the default C
  lmdb behaviour). On the other hand, non-duplicate values (i.e. no
  @DUPSORT), for which there is no sorting requirement, may be
  serialized more freely. For this purpose, using an encoding of
  :OCTETS or NIL with
  [cl-conspack](https://github.com/conspack/cl-conspack) is
  recommended because it works with complex objects, it encodes object
  types, it is fast and space-efficient, has a stable specification
  and an alternative implementation in C. For example:

  ```
  (with-temporary-env (env)
    (let ((db (get-db "test" :if-does-not-exist :create)))
      (with-txn (:write t)
        (put db "key1" (cpk:encode (list :some "stuff" 42)))
        (cpk:decode (g3t db "key1")))))
  => (:SOME "stuff" 42)
  ```
  """
  (@special-encodings section))

(defsection @special-encodings (:title "Special encodings")
  (*key-encoder* variable)
  (*key-decoder* variable)
  (*value-encoder* variable)
  (*value-decoder* variable)
  (with-mdb-val-slots macro)
  (%bytes-to-octets function))

(defvar *key-encoder* nil
  "A function designator or NIL. If non-NIL, it overrides the encoding
  method determined by KEY-ENCODING (see GET-DB). It is called with a
  single argument when a key is to be converted to an octet vector.")

(defvar *key-decoder* nil
  """A function designator or NIL. If non-NIL, it is called with a
  single MDB-VAL argument (see WITH-MDB-VAL-SLOTS), that holds a
  pointer to data to be decoded and its size. This function is called
  whenever a key is to be decoded and overrides the KEY-ENCODING
  argument of GET-DB.

  For example, if we are only interested in the length of the value
  and want to avoid creating a lisp vector on the heap, we can do
  this:

  ```
  (with-temporary-env (*env*)
    (let ((db (get-db "test" :if-does-not-exist :create)))
      (with-txn (:write t)
        (put db "key1" "abc")
        (let ((*value-decoder* (lambda (mdb-val)
                                 (with-mdb-val-slots (%bytes size mdb-val)
                                   (declare (ignore %bytes))
                                   ;; Take null termination into account.
                                   (1- size)))))
          (g3t db "key1")))))
  => 3
  => T
  ```
  """)

(defvar *value-encoder* nil
 "Like *KEY-ENCODER*, but for values.")

(defvar *value-decoder* nil
  "Like *KEY-DECODER*, but for values.

  Apart from performing actual decoding, the main purpose of
  *VALUE-DECODER*, one can also pass the foreign data on to other
  foreign functions such as `write()` directly from the decoder
  function and returning a constant such as T to avoid consing.")

(defmacro with-mdb-val-slots ((%bytes size mdb-val) &body body)
  "Bind %BYTES and SIZE locally to the corresponding slots of MDB-VAL.
  MDB-VAL is an opaque handle for a foreign `MDB_val` struct, that
  holds the pointer to a byte array and the number of bytes in the
  array. This macro is needed to access the foreign data in a function
  used as *KEY-DECODER* or *VALUE-DECODER*. MDB-VAL is dynamic extent,
  so don't hold on to it. Also, the pointer to which %BYTES is bound
  is valid only within the context of current top-level transaction."
  (alexandria:with-gensyms (%val)
    `(let* ((,%val (fixnum-to-aligned-pointer ,mdb-val))
            (,%bytes (cffi:foreign-slot-value ,%val '(:struct liblmdb:val)
                                              'liblmdb:mv-data))
            (,size (cffi:foreign-slot-value ,%val '(:struct liblmdb:val)
                                            'liblmdb:mv-size)))
       (declare (type cffi:foreign-pointer ,%bytes)
                (type fixnum ,size))
       ,@body)))

(deftype octet () '(unsigned-byte 8))
(deftype octets (&optional (size '*)) `(simple-array octet (,size)))

(cffi:defcfun ("memcpy" ) :int
  (dest :pointer)
  (src :pointer)
  (n liblmdb::size-t))

(defun %bytes-to-octets (mdb-val)
  "A utility function provided for writing *KEY-DECODER* and
  *VALUE-DECODER* functions. It returns a Lisp octet vector that holds
  the same bytes as MDB-VAL."
  (declare (type fixnum mdb-val))
  (with-mdb-val-slots (%bytes size mdb-val)
    ;; For small sizes, do it in Lisp to avoid the calling overhead.
    (if (< size 20)
        (let ((octets (make-array size :element-type 'octet)))
          (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
          (dotimes (i size)
            (setf (aref octets i) (cffi:mem-aref %bytes :unsigned-char i)))
          octets)
        (let ((octets (cffi:make-shareable-byte-vector size)))
          (declare (type octets octets))
          (cffi:with-pointer-to-vector-data (ptr octets)
            (memcpy ptr %bytes size))
          octets))))

(defun string-to-utf-8-bytes-with-null-termination (string)
  (trivial-utf-8:string-to-utf-8-bytes string :null-terminate t))

(defun utf-8-bytes-with-null-termination-to-string (aligned-%val)
  (let ((octets-with-null-termination (%bytes-to-octets aligned-%val)))
    (trivial-utf-8:utf-8-bytes-to-string
     octets-with-null-termination
     :end (1- (length octets-with-null-termination)))))

;;; Returning a CFFI shareable vector would reduce allocation and
;;; copying. Or use CFFI:WITH-FOREIGN-OBJECT. We'd need to know the
;;; size of the output for either, which we do for some encodings.
(defun encode-data (encoding data)
  (when (null encoding)
    (setq encoding (etypecase data
                     ((unsigned-byte 64) :uint64)
                     (string :utf-8)
                     (vector :octets))))
  (ecase encoding
    ((:uint64) (encode-native-uint64 data))
    ((:octets) data)
    ((:utf-8) (string-to-utf-8-bytes-with-null-termination data))))

(defun decode-data (encoding aligned-%val)
  (ecase encoding
    ((:uint64) (decode-native-uint64 aligned-%val))
    ((:octets nil) (%bytes-to-octets aligned-%val))
    ((:utf-8) (utf-8-bytes-with-null-termination-to-string aligned-%val))))

(defmacro encode-key (db data)
  (alexandria:once-only (data)
    `(if *key-encoder*
         (funcall (coerce *key-encoder* 'function) ,data)
         (encode-data (db-key-encoding ,db) ,data))))

(defmacro encode-value (db data)
  (alexandria:once-only (data)
    `(if *value-encoder*
         (funcall (coerce *value-encoder* 'function) ,data)
         (encode-data (db-value-encoding ,db) ,data))))

(defmacro decode-key (db %val)
  (alexandria:with-gensyms (mdb-val)
    `(let ((,mdb-val (aligned-pointer-to-fixnum ,%val)))
       (if *key-decoder*
           (funcall (coerce *key-decoder* 'function) ,mdb-val)
           (decode-data (db-key-encoding ,db) ,mdb-val)))))

(defmacro decode-value (db %val)
  (alexandria:with-gensyms (mdb-val)
    `(let ((,mdb-val (aligned-pointer-to-fixnum ,%val)))
       (if *value-decoder*
           (funcall (coerce *value-decoder* 'function) ,mdb-val)
           (decode-data (db-value-encoding ,db) ,mdb-val)))))

(defvar *endianness*
  #+little-endian :little-endian
  #+big-endian :big-endian)

(defun encode-native-uint64 (n)
  (declare (type (unsigned-byte 64) n)
           (optimize speed))
  (let ((octets (make-array 8 :element-type 'octet)))
    (declare (type octets octets))
    (if (eq *endianness* :little-endian)
        (progn
          (setf (aref octets 0) (ldb (byte 8 (* 0 8)) n))
          (setf (aref octets 1) (ldb (byte 8 (* 1 8)) n))
          (setf (aref octets 2) (ldb (byte 8 (* 2 8)) n))
          (setf (aref octets 3) (ldb (byte 8 (* 3 8)) n))
          (setf (aref octets 4) (ldb (byte 8 (* 4 8)) n))
          (setf (aref octets 5) (ldb (byte 8 (* 5 8)) n))
          (setf (aref octets 6) (ldb (byte 8 (* 6 8)) n))
          (setf (aref octets 7) (ldb (byte 8 (* 7 8)) n)))
        (progn
          (setf (aref octets 7) (ldb (byte 8 (* 0 8)) n))
          (setf (aref octets 6) (ldb (byte 8 (* 1 8)) n))
          (setf (aref octets 5) (ldb (byte 8 (* 2 8)) n))
          (setf (aref octets 4) (ldb (byte 8 (* 3 8)) n))
          (setf (aref octets 3) (ldb (byte 8 (* 4 8)) n))
          (setf (aref octets 2) (ldb (byte 8 (* 5 8)) n))
          (setf (aref octets 1) (ldb (byte 8 (* 6 8)) n))
          (setf (aref octets 0) (ldb (byte 8 (* 7 8)) n))))
    octets))

(defun decode-native-uint64 (aligned-%octets)
  "A function suitable for *KEY-DECODER* or *VALUE-DECODER* that
  decodes native unsigned 64 bit integers. This function is called
  automatically when the encoding is known to require it (see GET-DB's
  INTEGER-KEY, :VALUE-ENCODING, etc). It is provided to decode values
  manually."
  (declare (type fixnum aligned-%octets))
  (with-mdb-val-slots (%bytes size aligned-%octets)
    (declare (ignore size))
    (let ((n 0))
      (declare (type (unsigned-byte 64) n)
               (optimize speed))
      (if (eq *endianness* :little-endian)
          (loop for i below 8
                do (setf (ldb (byte 8 (* i 8)) n)
                         (cffi:mem-aref %bytes :unsigned-char i)))
          (loop for i below 8
                do (setf (ldb (byte 8 (* (- 7 i) 8)) n)
                         (cffi:mem-aref %bytes :unsigned-char i))))
      n)))

(declaim (inline init-%val))
(defun init-%val (%valp %bytes size)
  (setf (cffi:foreign-slot-value %valp '(:struct liblmdb:val) 'liblmdb:mv-data)
        %bytes)
  (setf (cffi:foreign-slot-value %valp '(:struct liblmdb:val) 'liblmdb:mv-size)
        size))

(declaim (inline copy-foreign-value))
(defun copy-foreign-value (%data n data)
  (declare (type fixnum n))
  (typecase data
    (octets
     (locally
         (declare (optimize speed (safety 0))
                  (octets data))
       (dotimes (i n)
         (setf (cffi:mem-aref %data :unsigned-char i) (aref data i)))))
    (simple-base-string
     (locally
         (declare (optimize speed (safety 0))
                  (simple-base-string data))
       (dotimes (i n)
         (setf (cffi:mem-aref %data :unsigned-char i)
               (char-code (aref data i))))))
    (t
     (dotimes (i n)
       (setf (cffi:mem-aref %data :unsigned-char i) (aref data i))))))

(defmacro with-val ((%valp data) &body body)
  (alexandria:once-only (data)
    (alexandria:with-gensyms (%data n with-val-body)
      `(cffi:with-foreign-object (,%valp '(:struct liblmdb:val))
         (declare (type vector ,data))
         (let ((,n (length ,data)))
           (flet ((,with-val-body ()
                    ,@body))
             (declare (dynamic-extent #',with-val-body))
             #+sbcl
             (if (typep ,data 'octets)
                 (cffi:with-pointer-to-vector-data (,%data ,data)
                   (init-%val ,%valp ,%data ,n)
                   (,with-val-body))
                 ;; This is the slow path. Silence the compiler notes.
                 (locally
                     (declare (optimize (speed 1)))
                   (cffi:with-foreign-object (,%data  :unsigned-char ,n)
                     (init-%val ,%valp ,%data ,n)
                     (copy-foreign-value ,%data ,n ,data)
                     (,with-val-body))))
             #-sbcl
             (cffi:with-foreign-object (,%data  :unsigned-char ,n)
               (init-%val ,%valp ,%data ,n)
               (copy-foreign-value ,%data ,n ,data)
               (,with-val-body))))))))

(defmacro with-empty-val ((%valp) &body body)
  `(cffi:with-foreign-object (,%valp '(:struct liblmdb:val))
     ,@body))


(defsection @basic-operations (:title "Basic operations")
  (g3t function)
  (put function)
  (del function))

(defun g3t (db key)
  "Return the value from DB associated with KEY and T as the second
  value. If KEY is not found in DB, then NIL is returned. If DB
  supports @DUPSORT, then the first value for KEY will be returned.
  Retrieval of other values requires the use of @CURSORS.

  This function is called G3T instead of GET to avoid having to shadow
  CL:GET when importing LMDB.

  Wraps [mdb_get()](http://www.lmdb.tech/doc/group__mdb.html#ga8bf10cd91d3f3a83a34d04ce6b07992d)."
  (declare (optimize speed))
  (async-signal-safe
    (with-val (%key (encode-key db key))
      (with-empty-val (%val)
        (let ((return-code (liblmdb:get (open-%txnp-for-db db) (%dbi db)
                                        %key %val)))
          (alexandria:switch (return-code)
            ;; This is the only thing that conses here with raw octets ...
            (0 (values (decode-value db %val) t))
            (liblmdb:+notfound+ (values nil nil))
            (t (lmdb-error return-code))))))))

(defun put (db key value &key (overwrite t) (dupdata t) append append-dup)
  "Add a KEY, VALUE pair to DB within TXN (which must support writes).
  Return VALUE.

  - OVERWRITE: If NIL, signal LMDB-KEY-EXISTS-ERROR if KEY already
    appears in DB.

  - DUPDATA: If NIL, signal LMDB-KEY-EXISTS-ERROR if the KEY, VALUE
    pair already appears in DB. Has no effect if DB doesn't have
    DUPSORT.

  - APPEND: Append the KEY, VALUE pair to the end of DB instead of
    finding KEY's location in the B+ tree by performing comparisons.
    The client effectively promises that keys are inserted in sort
    order, which allows for fast bulk loading. If the promise is
    broken, a LMDB-KEY-EXISTS-ERROR is signalled.

  - APPEND-DUP: The client promises that duplicate values are inserted
    in sort order. If the promise is broken, a LMDB-KEY-EXISTS-ERROR
    is signalled.

  May signal LMDB-MAP-FULL-ERROR, LMDB-TXN-FULL-ERROR,
  LMDB-TXN-READ-ONLY-ERROR.

  Wraps [mdb_put()](http://www.lmdb.tech/doc/group__mdb.html#ga4fa8573d9236d54687c61827ebf8cac0)."
  (without-interrupts
    (with-val (%key (encode-key db key))
      (with-val (%val (encode-value db value))
        (let ((return-code
                (liblmdb:put (open-%txnp-for-db db) (%dbi db) %key %val
                             (logior
                              (if dupdata 0 liblmdb:+nodupdata+)
                              (if overwrite 0 liblmdb:+nooverwrite+)
                              (if append liblmdb:+append+ 0)
                              (if append-dup liblmdb:+appenddup+ 0)))))
          (alexandria:switch (return-code)
            (0 value)
            (+eacces+ (lmdb-error +txn-read-only+))
            (t (lmdb-error return-code))))))))

(defun del (db key &key value)
  "Delete KEY from DB. Returns T if data was deleted, NIL otherwise.
  If DB supports sorted duplicates (@DUPSORT), then VALUE is taken
  into account: if it's NIL, then all duplicate values for KEY are
  deleted, if it's not NIL, then only the matching value. May signal
  LMDB-TXN-READ-ONLY-ERROR.

  Wraps [mdb_del()](http://www.lmdb.tech/doc/group__mdb.html#gab8182f9360ea69ac0afd4a4eaab1ddb0)."
  (flet ((handle-return-code (return-code)
           (alexandria:switch (return-code)
             (0 t)
             (liblmdb:+notfound+ nil)
             (+eacces+ (lmdb-error +txn-read-only+))
             (t (lmdb-error return-code)))))
    (without-interrupts
      (if (null value)
          (with-val (%key (encode-key db key))
            (let ((return-code (liblmdb:del (open-%txnp-for-db db) (%dbi db)
                                            %key +null-pointer+)))
              (handle-return-code return-code)))
          (with-val (%key (encode-key db key))
            (with-val (%val (encode-value db value))
              (let ((return-code (liblmdb:del (open-%txnp-for-db db) (%dbi db)
                                              %key %val)))
                (handle-return-code return-code))))))))


(defsection @cursors (:title "Cursors")
  (with-cursor macro)
  (with-implicit-cursor macro)
  (cursor class)
  (cursor-db function)
  (@default-cursor glossary-term)
  (@positioning-cursors section)
  (@basic-cursor-operations section)
  (@misc-cursor section))

(declaim (inline make-cursor))
(defstruct cursor
  %cursorp
  (txn nil :type txn)
  (env nil :type env)
  (db nil :type db)
  thread)

(declaim (inline %cursorp))
(defun %cursorp (cursor)
  (fixnum-to-aligned-pointer (cursor-%cursorp cursor)))

(declaim (inline check-cursor))
(defun check-cursor (cursor)
  (declare (optimize speed))
  (let* ((thread (cursor-thread cursor))
         (in-other-thread (not (eq thread (bt:current-thread))))
         (txn (cursor-txn cursor)))
    (when in-other-thread
      (lmdb-error +cursor-thread+))
    (unless (%open-txn-p txn)
      (lmdb-error liblmdb:+bad-txn+))
    (let ((active-txn (if (eq txn *txn*)
                          txn
                          (active-txn-in-env (cursor-env cursor) t))))
      (declare (type txn txn active-txn))
      (unless (eq txn active-txn)
        (if (%open-txn-p active-txn)
            (lmdb-error +illegal-access-to-parent-txn+)
            (lmdb-error liblmdb:+bad-txn+))))))

(defun %open-cursor (db)
  (declare (optimize speed))
  ;; WITHOUT-INTERRUPTS is in WITH-CURSOR.
  (cffi:with-foreign-object (%cursorpp :pointer)
    (let ((return-code (liblmdb:cursor-open (open-%txnp-for-db db) (%dbi db)
                                            %cursorpp)))
      (if (zerop return-code)
          (aligned-pointer-to-fixnum (cffi:mem-ref %cursorpp :pointer))
          (lmdb-error return-code)))))

(defun close-cursor (cursor)
  (declare (optimize speed))
  ;; No need to CHECK-CURSOR since this is only called in WITH-CURSOR.
  ;; WITH-CURSOR takes care of WITHOUT-INTERRUPTS, too.
  ;; This is a void C function.
  (liblmdb:cursor-close (%cursorp cursor))
  (setf (cursor-%cursorp cursor) nil))

(defmacro with-cursor ((var db) &body body)
  "Bind VAR to a fresh CURSOR on DB. Execute BODY, then close the
  cursor. Within the dynamic extent of BODY, this will be the
  @DEFAULT-CURSOR. The cursor is tied to the @ACTIVE-TRANSACTION.

  LMDB-CURSOR-THREAD-ERROR is signalled if the cursor is accessed from
  threads other than the one in which it was created.

  Wraps [mdb_cursor_open()](http://www.lmdb.tech/doc/group__mdb.html#ga9ff5d7bd42557fd5ee235dc1d62613aa)
  and [mdb_cursor_close()](http://www.lmdb.tech/doc/group__mdb.html#gad685f5d73c052715c7bd859cc4c05188)."
  (alexandria:with-gensyms (with-cursor-body)
    `(flet ((,with-cursor-body (,var)
              ,@body))
       (declare (dynamic-extent #',with-cursor-body))
       (call-with-cursor #',with-cursor-body ,db))))

(defvar *default-cursor* nil)
#+sbcl
(declaim (sb-ext:always-bound *default-cursor*))

(defun call-with-cursor (fn db)
  (declare (type function fn)
           (optimize speed))
  (let* ((env (db-env db))
         (active-txn (active-txn-in-env env)))
    (without-interrupts
      (let ((*default-cursor* (make-cursor :%cursorp (%open-cursor db)
                                           :env env :txn active-txn :db db
                                           :thread (bt:current-thread))))
        (unwind-protect
             (with-interrupts (funcall fn *default-cursor*))
          (close-cursor *default-cursor*))))))

(defmacro with-implicit-cursor ((db) &body body)
  "Like WITH-CURSOR, but the cursor object is not accessible directly,
  only through the @DEFAULT-CURSOR mechanism. The cursor is
  stack-allocated, which eliminates the consing of WITH-CURSOR. Note
  that stack allocation of cursors in WITH-CURSOR would risk data
  corruption if the cursor were accessed beyond its dynamic extent.

  Use WITH-IMPLICIT-CURSOR instead of WITH-CURSOR if a single cursor
  at a time will suffice. Conversely, use WITH-CURSOR if a second
  cursor is needed. That is, use

  ```
  (with-implicit-cursor (db)
    (cursor-set-key 1))
  ```

  but when two cursors iterate in an interleaved manner, use
  WITH-CURSOR:

  ```
  (with-cursor (c1 db)
    (with-cursor (c2 db)
      (cursor-first c1)
      (cursor-last c2)
      (if (some-pred (cursor-value c1) (cursor-value c2))
          (cursor-next c1)
          (cursor-prev c2))
      ...))
  ```

  Wraps [mdb_cursor_open()](http://www.lmdb.tech/doc/group__mdb.html#ga9ff5d7bd42557fd5ee235dc1d62613aa)
  and [mdb_cursor_close()](http://www.lmdb.tech/doc/group__mdb.html#gad685f5d73c052715c7bd859cc4c05188)."
  (alexandria:with-gensyms (with-cursor-body)
    `(flet ((,with-cursor-body ()
              ,@body))
       (declare (dynamic-extent #',with-cursor-body))
       (call-with-implicit-cursor #',with-cursor-body ,db))))

(defun call-with-implicit-cursor (fn db)
  (declare (type function fn)
           (optimize speed))
  (without-interrupts
    (let* ((env (db-env db))
           (active-txn (active-txn-in-env env t))
           (*default-cursor* (make-cursor :%cursorp (%open-cursor db)
                                          :env env :txn active-txn :db db
                                          :thread (bt:current-thread))))
      (declare (type cursor *default-cursor*)
               (dynamic-extent *default-cursor*))
      (unwind-protect
           (with-interrupts (funcall fn))
        (close-cursor *default-cursor*)))))

(define-glossary-term @default-cursor (:title "default cursor")
  "All operations, described below, that take cursor arguments accept
  NIL instead of a CURSOR object, in which case the cursor from the
  immediately enclosing WITH-CURSOR or WITH-IMPLICIT-CURSOR is used.
  This cursor is referred to as the _default cursor_.

  To reduce syntactic clutter, some operations thus make cursor
  arguments &OPTIONAL. When this is undesirable - because there are
  keyword arguments as well - the cursor may be a required argument as
  in CURSOR-PUT. Still NIL can be passed explicitly.")

(defsection @positioning-cursors (:title "Positioning cursors")
  "The following functions *position* or *initialize* a cursor while
  returning the value (*a* value with @DUPSORT) associated with a key,
  or both the key and the value. Initialization is successful if there
  is the cursor points to a key-value pair, which is indicated by the
  last return value being T."
  (cursor-first function)
  (cursor-first-dup function)
  (cursor-last function)
  (cursor-last-dup function)
  (cursor-next function)
  (cursor-next-nodup function)
  (cursor-next-dup function)
  (cursor-prev function)
  (cursor-prev-nodup function)
  (cursor-prev-dup function)
  (cursor-set-key function)
  (cursor-set-key-dup function)
  (cursor-set-range function)
  (cursor-set-range-dup function))

(defun cursor-first (&optional cursor)
  "Move CURSOR to the first key of its database. Return the key, the
  value and T, or NIL if the database is empty. If @DUPSORT, position
  CURSOR on the first value of the first key.

  Wraps [mdb_cursor_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
  with [MDB_FIRST](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127)."
  (let ((cursor (or cursor *default-cursor*)))
    (check-cursor cursor)
    (async-signal-safe
      (with-empty-val (%key)
        (with-empty-val (%val)
          (let ((return-code (liblmdb:cursor-get (%cursorp cursor) %key %val
                                                 :+first+)))
            (alexandria:switch (return-code)
              (0 (values (decode-key (cursor-db cursor) %key)
                         (decode-value (cursor-db cursor) %val)
                         t))
              (liblmdb:+notfound+ nil)
              (t (lmdb-error return-code)))))))))

(defun cursor-first-dup (&optional cursor)
  "Move CURSOR to the first duplicate value of the current key. Return
  the value and T. Return NIL if CURSOR is not positioned.

  Wraps [mdb_cursor_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
  with [MDB_FIRST_DUP](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127)."
  (let ((cursor (or cursor *default-cursor*)))
    (check-cursor cursor)
    (async-signal-safe
      (with-empty-val (%key)
        (with-empty-val (%val)
          (let ((return-code (liblmdb:cursor-get (%cursorp cursor) %key %val
                                                 :+first-dup+)))
            (alexandria:switch (return-code)
              (0 (values (decode-value (cursor-db cursor) %val) t))
              (+einval+ nil)
              ;; More conditions?
              (t (lmdb-error return-code)))))))))

(defun cursor-last (&optional cursor)
  "Move CURSOR to the last key of its database. Return the key, the
  value and T, or NIL if the database is empty. If @DUPSORT, position
  CURSOR on the last value of the last key.

  Wraps [mdb_cursor_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
  with [MDB_LAST](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127)."
  (let ((cursor (or cursor *default-cursor*)))
    (check-cursor cursor)
    (async-signal-safe
      (with-empty-val (%key)
        (with-empty-val (%val)
          (let ((return-code (liblmdb:cursor-get (%cursorp cursor) %key %val
                                                 :+last+)))
            (alexandria:switch (return-code)
              (0 (values (decode-key (cursor-db cursor) %key)
                         (decode-value (cursor-db cursor) %val)
                         t))
              (liblmdb:+notfound+ nil)
              (t (lmdb-error return-code)))))))))

(defun cursor-last-dup (&optional cursor)
  "Move CURSOR to the last duplicate value of the current key. Return
  the value and T. Return NIL if CURSOR is not positioned.

  Wraps [mdb_cursor_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
  with [MDB_LAST_DUP](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127)."
  (let ((cursor (or cursor *default-cursor*)))
    (check-cursor cursor)
    (async-signal-safe
      (with-empty-val (%key)
        (with-empty-val (%val)
          (let ((return-code (liblmdb:cursor-get (%cursorp cursor) %key %val
                                                 :+last-dup+)))
            (alexandria:switch (return-code)
              (0 (values (decode-value (cursor-db cursor) %val) t))
              (+einval+ nil)
              (t (lmdb-error return-code)))))))))

(defun cursor-next (&optional cursor)
  "Move CURSOR to the next key-value pair of its database and return
  the key, the value, and T. Return NIL if there is no next item. If
  @DUPSORT, position CURSOR on the next value of the current key if
  exists, else the first value of next key. If CURSOR is
  uninitialized, then CURSOR-FIRST is called on it first.

  Wraps [mdb_cursor_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
  with [MDB_NEXT](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127)."
  (let ((cursor (or cursor *default-cursor*)))
    (check-cursor cursor)
    (async-signal-safe
      (with-empty-val (%key)
        (with-empty-val (%val)
          (let ((return-code (liblmdb:cursor-get (%cursorp cursor) %key %val
                                                 :+next+)))
            (alexandria:switch (return-code)
              (0 (values (decode-key (cursor-db cursor) %key)
                         (decode-value (cursor-db cursor) %val)
                         t))
              (liblmdb:+notfound+ nil)
              (t (lmdb-error return-code)))))))))

(defun cursor-next-nodup (&optional cursor)
  "Move CURSOR to the first value of next key pair of its
  database (skipping over duplicate values of the current key). Return
  the key, the value and T. Return NIL if there is no next item. If
  CURSOR is uninitialized, then CURSOR-FIRST is called on it first.

  Wraps [mdb_cursor_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
  with [MDB_NEXT_NODUP](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127)."
  (let ((cursor (or cursor *default-cursor*)))
    (check-cursor cursor)
    (async-signal-safe
      (with-empty-val (%key)
        (with-empty-val (%val)
          (let ((return-code (liblmdb:cursor-get (%cursorp cursor) %key %val
                                                 :+next-nodup+)))
            (alexandria:switch (return-code)
              (0 (values (decode-key (cursor-db cursor) %key)
                         (decode-value (cursor-db cursor) %val)
                         t))
              (liblmdb:+notfound+ nil)
              (t (lmdb-error return-code)))))))))

(defun cursor-next-dup (&optional cursor)
  "Move CURSOR to the next value of current key pair of its database.
  Return the value and T. Return NIL if there is no next value. If
  CURSOR is uninitialized, then CURSOR-FIRST is called on it first.

  Wraps [mdb_cursor_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
  with [MDB_NEXT_DUP](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127)."
  (let ((cursor (or cursor *default-cursor*)))
    (check-cursor cursor)
    (async-signal-safe
      (with-empty-val (%key)
        (with-empty-val (%val)
          (let ((return-code (liblmdb:cursor-get (%cursorp cursor) %key %val
                                                 :+next-dup+)))
            (alexandria:switch (return-code)
              (0 (values (decode-value (cursor-db cursor) %val) t))
              (liblmdb:+notfound+ nil)
              (t (lmdb-error return-code)))))))))

(defun cursor-prev (&optional cursor)
  "Move CURSOR to the previous key-value pair of its database.
  Return the key, the value and T. Return NIL if there is no previous
  item. If @DUPSORT, position CURSOR on the previous value of the
  current key if exists, else the last value of previous key. If
  CURSOR is uninitialized, then CURSOR-LAST is called on it first.

  Wraps [mdb_cursor_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
  with [MDB_PREV](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127)."
  (let ((cursor (or cursor *default-cursor*)))
    (check-cursor cursor)
    (async-signal-safe
      (with-empty-val (%key)
        (with-empty-val (%val)
          (let ((return-code (liblmdb:cursor-get (%cursorp cursor) %key %val
                                                 :+prev+)))
            (alexandria:switch (return-code)
              (0 (values (decode-key (cursor-db cursor) %key)
                         (decode-value (cursor-db cursor) %val)
                         t))
              (liblmdb:+notfound+ nil)
              (t (lmdb-error return-code)))))))))

(defun cursor-prev-nodup (&optional cursor)
  "Move CURSOR to the last value of previous key pair of its
  database (skipping over duplicate values of the current and the
  previous key). Return the key, the value, and T. Return NIL if
  there is no prev item. If CURSOR is uninitialized, then CURSOR-LAST
  is called on it first.

  Wraps [mdb_cursor_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
  with [MDB_PREV_NODUP](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127)."
  (let ((cursor (or cursor *default-cursor*)))
    (check-cursor cursor)
    (async-signal-safe
      (with-empty-val (%key)
        (with-empty-val (%val)
          (let ((return-code (liblmdb:cursor-get (%cursorp cursor) %key %val
                                                 :+prev-nodup+)))
            (alexandria:switch (return-code)
              (0 (values (decode-key (cursor-db cursor) %key)
                         (decode-value (cursor-db cursor) %val)
                         t))
              (liblmdb:+notfound+ nil)
              (t (lmdb-error return-code)))))))))

(defun cursor-prev-dup (&optional cursor)
  "Move CURSOR to the previous duplicate value of current key pair of
  its database. Return the value and T. Return NIL if there is no prev
  value. If CURSOR is uninitialized, then CURSOR-LAST is called on it
  first.

  Wraps [mdb_cursor_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
  with [MDB_PREV_DUP](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127)."
  (let ((cursor (or cursor *default-cursor*)))
    (check-cursor cursor)
    (async-signal-safe
      (with-empty-val (%key)
        (with-empty-val (%val)
          (let ((return-code (liblmdb:cursor-get (%cursorp cursor) %key %val
                                                 :+prev-dup+)))
            (alexandria:switch (return-code)
              (0 (values (decode-value (cursor-db cursor) %val) t))
              (liblmdb:+notfound+ nil)
              (t (lmdb-error return-code)))))))))

(defun cursor-set-key (key &optional cursor)
  "Move CURSOR to KEY of its database. Return the corresponding value
  and T. Return NIL if KEY was not found. If @DUPSORT, position CURSOR
  on the first value of KEY.

  Wraps [mdb_cursor_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
  with [MDB_SET_KEY](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127)."
  (declare (optimize speed))
  (let ((cursor (or cursor *default-cursor*)))
    (check-cursor cursor)
    (async-signal-safe
      (let ((db (cursor-db cursor)))
        (with-val (%key (encode-key db key))
          (with-empty-val (%val)
            (let ((return-code (liblmdb:cursor-get (%cursorp cursor) %key %val
                                                   :+set-key+)))
              (alexandria:switch (return-code)
                (0 (values (decode-value db %val) t))
                (liblmdb:+notfound+ nil)
                (t (lmdb-error return-code))))))))))

(defun cursor-set-key-dup (key value &optional cursor)
  "Move CURSOR to the KEY, VALUE pair of its database and return T on
  success. Return NIL if the pair was not found.

  Wraps [mdb_cursor_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
  with [MDB_GET_BOTH](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127)."
  (let ((cursor (or cursor *default-cursor*)))
    (check-cursor cursor)
    (async-signal-safe
      (with-val (%key (encode-key (cursor-db cursor) key))
        (with-val (%val (encode-value (cursor-db cursor) value))
          (let ((return-code (liblmdb:cursor-get (%cursorp cursor) %key %val
                                                 :+get-both+)))
            (alexandria:switch (return-code)
              (0 t)
              (liblmdb:+notfound+ nil)
              (t (lmdb-error return-code)))))))))

(defun cursor-set-range (key &optional cursor)
  "Position CURSOR on the first key equal to or greater than KEY.
  Return the found key, the value and T. Return NIL if KEY was not
  found. If @DUPSORT, position CURSOR on the first value of the found
  key.

  Wraps [mdb_cursor_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
  with [MDB_SET_RANGE](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127)."
  (let ((cursor (or cursor *default-cursor*)))
    (check-cursor cursor)
    (async-signal-safe
      (with-val (%key (encode-key (cursor-db cursor) key))
        (with-empty-val (%val)
          (let ((return-code (liblmdb:cursor-get (%cursorp cursor) %key %val
                                                 :+set-range+)))
            (alexandria:switch (return-code)
              (0 (values (decode-key (cursor-db cursor) %key)
                         (decode-value (cursor-db cursor) %val)
                         t))
              (liblmdb:+notfound+ nil)
              (t (lmdb-error return-code)))))))))

(defun cursor-set-range-dup (key value &optional cursor)
  "Position CURSOR exactly at KEY on the first value greater than or
  equal to VALUE. Return the value at the position and T on success,
  or NIL if there is no such value associated with KEY.

  Wraps [mdb_cursor_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
  with [MDB_GET_BOTH_RANGE](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127)."
  (let ((cursor (or cursor *default-cursor*)))
    (check-cursor cursor)
    (async-signal-safe
      (with-val (%key (encode-key (cursor-db cursor) key))
        (with-val (%val (encode-value (cursor-db cursor) value))
          (let ((return-code (liblmdb:cursor-get (%cursorp cursor) %key %val
                                                 :+get-both-range+)))
            (alexandria:switch (return-code)
              (0 (values (decode-value (cursor-db cursor) %val) t))
              (liblmdb:+notfound+ nil)
              (t (lmdb-error return-code)))))))))

(defsection @basic-cursor-operations (:title "Basic cursor operations")
  "The following operations are similar to G3T, PUT, DEL (the
  @BASIC-OPERATIONS), but G3T has three variants (CURSOR-KEY-VALUE,
  CURSOR-KEY, and CURSOR-VALUE). All of them require the cursor to be
  positioned (see @POSITIONING-CURSORS)."
  (cursor-key-value function)
  (cursor-key function)
  (cursor-value function)
  (cursor-put function)
  (cursor-del function))

(defun cursor-key-value (&optional cursor)
  "Return the key and value CURSOR is positioned at and T. Return NIL
  if CURSOR is uninitialized.

  Wraps [mdb_cursor_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
  with [MDB_GET_CURRENT](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127)."
  (let ((cursor (or cursor *default-cursor*)))
    (check-cursor cursor)
    (async-signal-safe
      (with-empty-val (%key)
        (with-empty-val (%val)
          (let ((return-code (liblmdb:cursor-get (%cursorp cursor) %key %val
                                                 :+get-current+)))
            (alexandria:switch (return-code)
              (0 (values (decode-key (cursor-db cursor) %key)
                         (decode-value (cursor-db cursor) %val)
                         t))
              (+einval+ nil)
              (t (lmdb-error return-code)))))))))

(defun cursor-key (&optional cursor)
  "Return the key CURSOR is positioned at and T. Return NIL if CURSOR
  is uninitialized.

  Wraps [mdb_cursor_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
  with [MDB_GET_CURRENT](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127)."
  (let ((cursor (or cursor *default-cursor*)))
    (check-cursor cursor)
    (async-signal-safe
      (with-empty-val (%key)
        (with-empty-val (%val)
          (let ((return-code (liblmdb:cursor-get (%cursorp cursor) %key %val
                                                 :+get-current+)))
            (alexandria:switch (return-code)
              (0 (values (decode-key (cursor-db cursor) %key) t))
              (+einval+ nil)
              (t (lmdb-error return-code)))))))))

(defun cursor-value (&optional cursor)
  "Return the value CURSOR is positioned at and T. Return NIL if
  CURSOR is uninitialized.

  Wraps [mdb_cursor_get()](http://www.lmdb.tech/doc/group__mdb.html#ga48df35fb102536b32dfbb801a47b4cb0)
  with [MDB_GET_CURRENT](http://www.lmdb.tech/doc/group__mdb.html#ga1206b2af8b95e7f6b0ef6b28708c9127)."
  (let ((cursor (or cursor *default-cursor*)))
    (check-cursor cursor)
    (async-signal-safe
      (with-empty-val (%key)
        (with-empty-val (%val)
          (let ((return-code (liblmdb:cursor-get (%cursorp cursor) %key %val
                                                 :+get-current+)))
            (alexandria:switch (return-code)
              (0 (values (decode-value (cursor-db cursor) %val) t))
              (+einval+ nil)
              (t (lmdb-error return-code)))))))))

(defun cursor-put (key value cursor &key current
                   (overwrite t) (dupdata t) append append-dup)
  "Like PUT, store key-value pairs into CURSOR's database.
  CURSOR is positioned at the new item, or on failure usually near it.
  Return VALUE.

  - CURRENT: Replace the item at the current cursor position. KEY must
    still be provided, and must match it. If using sorted
    duplicates (@DUPSORT), VALUE must still sort into the same place.
    This is intended to be used when the new data is the same size as
    the old. Otherwise it will simply perform a delete of the old
    record followed by an insert.

  - OVERWRITE: If NIL, signal LMDB-KEY-EXISTS-ERROR if KEY already
    appears in CURSOR-DB.

  - DUPDATA: If NIL, signal LMDB-KEY-EXISTS-ERROR if the KEY, VALUE
    pair already appears in DB. Has no effect if CURSOR-DB doesn't
    have @DUPSORT.

  - APPEND: Append the KEY, VALUE pair to the end of CURSOR-DB instead
    of finding KEY's location in the B+ tree by performing
    comparisons. The client effectively promises that keys are
    inserted in sort order, which allows for fast bulk loading. If the
    promise is broken, LMDB-KEY-EXISTS-ERROR is signalled.

  - APPEND-DUP: The client promises that duplicate values are inserted
    in sort order. If the promise is broken, LMDB-KEY-EXISTS-ERROR is
    signalled.

  May signal LMDB-MAP-FULL-ERROR, LMDB-TXN-FULL-ERROR,
  LMDB-TXN-READ-ONLY-ERROR.

  Wraps [mdb_cursor_put()](http://www.lmdb.tech/doc/group__mdb.html#ga1f83ccb40011837ff37cc32be01ad91e)."
  (let ((cursor (or cursor *default-cursor*)))
    (check-cursor cursor)
    (without-interrupts
      (with-val (%key (encode-key (cursor-db cursor) key))
        (with-val (%val (encode-value (cursor-db cursor) value))
          (let ((return-code (liblmdb:cursor-put
                              (%cursorp cursor) %key %val
                              (logior
                               (if current liblmdb:+current+ 0)
                               (if dupdata 0 liblmdb:+nodupdata+)
                               (if overwrite 0 liblmdb:+nooverwrite+)
                               (if append liblmdb:+append+ 0)
                               (if append-dup liblmdb:+appenddup+ 0)))))
            (alexandria:switch (return-code)
              (0 value)
              (+eacces+ (lmdb-error +txn-read-only+))
              (t (lmdb-error return-code)))))))))

(defun cursor-del (cursor &key delete-dups)
  "Delete the key-value pair CURSOR is positioned at. This does not
  make the cursor uninitialized, so operations such as CURSOR-NEXT can
  still be used on it. Both CURSOR-NEXT and CURSOR-KEY-VALUE will
  return the same record after this operation. If CURSOR is not
  initialized, LMDB-CURSOR-UNINITIALIZED-ERROR is signalled. Returns
  no values.

  If DELETE-DUPS, delete all duplicate values that belong to the
  current key. With DELETE-DUPS, CURSOR-DB must have @DUPSORT, else
  LMDB-INCOMPATIBLE-ERROR is signalled.

  May signal LMDB-CURSOR-UNINITIALIZED-ERROR,
  LMDB-TXN-READ-ONLY-ERROR.

  Wraps [mdb_cursor_del()](http://www.lmdb.tech/doc/group__mdb.html#ga26a52d3efcfd72e5bf6bd6960bf75f95)."
  (let ((cursor (or cursor *default-cursor*)))
    (check-cursor cursor)
    (without-interrupts
      (let ((return-code
              (liblmdb:cursor-del (%cursorp cursor)
                                  (if delete-dups liblmdb:+nodupdata+ 0))))
        (alexandria:switch (return-code)
          (0 (values))
          (+einval+ (lmdb-error +cursor-uninitialized+))
          (+eacces+ (lmdb-error +txn-read-only+))
          (liblmdb:+incompatible+
           (lmdb-error return-code "DB does not have DUPSORT."))
          (t (lmdb-error return-code)))))))


(defsection @misc-cursor (:title "Miscellaneous cursor operations")
  (cursor-renew function)
  (cursor-count function)
  (do-cursor macro)
  (do-cursor-dup macro)
  (do-db macro)
  (do-db-dup macro)
  (list-dups function))

(defun cursor-renew (&optional cursor)
  "Associate CURSOR with the @ACTIVE-TRANSACTION (which must be
  read-only) as if it had been created with that transaction to begin
  with to avoid allocation overhead. CURSOR-DB stays the same. This
  may be done whether the previous transaction is open or closed (see
  OPEN-TXN-P). No values are returned.

  Wraps [mdb_cursor_renew()](http://www.lmdb.tech/doc/group__mdb.html#gac8b57befb68793070c85ea813df481af)."
  (let ((cursor (or cursor *default-cursor*)))
    (without-interrupts
      (let ((return-code (liblmdb:cursor-renew (open-%txnp-for-db
                                                (cursor-db cursor))
                                               (%cursorp cursor))))
        (alexandria:switch (return-code)
          (0 (setf (cursor-thread cursor) (bt:current-thread))
             (setf (cursor-txn cursor)
                   (active-txn-in-env (cursor-env cursor) t))
             (values))
          (t (lmdb-error return-code)))))))

(defun cursor-count (&optional cursor)
  "Return the number of duplicate values for the current key of
  CURSOR. If CURSOR-DB doesn't have @DUPSORT, LMDB-INCOMPATIBLE-ERROR
  is signalled. If CURSOR is not initialized,
  LMDB-CURSOR-UNINITIALIZED-ERROR is signalled.

  Wraps [mdb_cursor_count()](http://www.lmdb.tech/doc/group__mdb.html#ga4041fd1e1862c6b7d5f10590b86ffbe2)."
  (let ((cursor (or cursor *default-cursor*)))
    (check-cursor cursor)
    (async-signal-safe
      (cffi:with-foreign-object (%countp 'liblmdb::size-t)
        (let ((return-code (liblmdb:cursor-count (%cursorp cursor) %countp)))
          (alexandria:switch (return-code)
            (0 (cffi:mem-ref %countp 'liblmdb::size-t))
            (+einval+ (lmdb-error +cursor-uninitialized+))
            (liblmdb:+incompatible+
             (lmdb-error return-code "DB does not have DUPSORT."))
            (t (lmdb-error return-code))))))))

(defmacro do-cursor ((key-var value-var cursor &key from-end nodup)
                     &body body)
  "Iterate over key-value pairs starting from the position of CURSOR.
  If CURSOR is not positioned then no key-value pairs will be seen. If
  FROM-END, then iterate with CURSOR-PREV instead of CURSOR-NEXT. If
  NODUP, then make that CURSOR-PREV-NODUP and CURSOR-NEXT-NODUP.

  If CURSOR is NIL, the @DEFAULT-CURSOR is used.

  If NODUP and not FROM-END, then the first duplicate of each key will
  be seen. If NODUP and FROM-END, then the last duplicate of each key
  will be seen.

  To iterate over all key-value pairs with keys >= 7:

  ```
  (with-cursor (cursor db)
    (cursor-set-key 7 cursor)
    (do-cursor (key value cursor)
      (print (cons key value))))
  ```"
  (alexandria:once-only (cursor from-end nodup)
    `(multiple-value-bind (,key-var ,value-var) (cursor-key-value ,cursor)
       (loop while ,key-var do
         (progn ,@body)
         (multiple-value-setq (,key-var ,value-var)
           (if ,from-end
               (if ,nodup
                   (cursor-prev-nodup ,cursor)
                   (cursor-prev ,cursor))
               (if ,nodup
                   (cursor-next-nodup ,cursor)
                   (cursor-next ,cursor))))))))

(defmacro do-cursor-dup ((value-var cursor &key from-end) &body body)
  "Iterate over duplicate values with starting from the position of
  CURSOR. If CURSOR is not positioned then no values will be seen. If
  FROM-END, then iterate with CURSOR-PREV-DUP instead of
  CURSOR-NEXT-DUP.

  If CURSOR is NIL, the @DEFAULT-CURSOR is used.

  To iterate over all values that not smaller than #(3 4 5),
  associated with the key 7:

  ```
  (with-cursor (cursor db)
    (cursor-set-key-dup cursor 7 #(3 4 5))
    (do-cursor-dup (value cursor)
      (print value)))
  ```"
  (alexandria:once-only (cursor from-end)
    `(let ((,value-var (cursor-value ,cursor)))
       (loop while ,value-var do
         (progn ,@body)
         (setq ,value-var (if ,from-end
                              (cursor-prev-dup ,cursor)
                              (cursor-next-dup ,cursor)))))))

(defmacro do-db ((key-var value-var db &key from-end nodup) &body body)
  "Iterate over all keys and values in DB. If NODUP, then all but the
  first (or last if FROM-END) value for each key are skipped. If
  FROM-END, then iterate in reverse order.

  To iterate over all values in DB:

  ```
  (do-db (key value db)
    (print (cons key value)))
  ```

  This macro establishes a @DEFAULT-CURSOR."
  (alexandria:once-only (from-end nodup)
    `(with-implicit-cursor (,db)
       (if ,from-end
           (cursor-last)
           (cursor-first))
       (do-cursor (,key-var ,value-var *default-cursor*
                            :from-end ,from-end :nodup ,nodup)
         ,@body))))

(defmacro do-db-dup ((value-var db key &key from-end) &body body)
  "Iterate over all values associated with KEY in DB. If FROM-END,
  then iteration starts at the largest value.

  To iterate over all values associated with the key 7:

  ```
  (do-db-dup (value db 7)
    (print value))
  ```

  This macro establishes a @DEFAULT-CURSOR."
  (alexandria:once-only (from-end)
    `(with-implicit-cursor (,db)
       (when (nth-value 1 (cursor-set-key ,key))
         (when ,from-end
           (cursor-last-dup))
         (do-cursor-dup (,value-var *default-cursor* :from-end ,from-end)
           ,@body)))))

(defun list-dups (db key &key from-end)
  "A thin wrapper around DO-DB-DUP, this function returns all values
  associated with KEY in DB as a list. If FROM-END, then the first
  element of the list is the largest value."
  (let ((dups ()))
    (do-db-dup (value db key :from-end (not from-end))
      (push value dups))))
