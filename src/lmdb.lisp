(in-package :cl-user)
(defpackage lmdb
  (:use :cl)
  (:shadow :get)
  ;; Classes
  (:export :environment
           :make-environment
           :transaction
           :transaction-environment
           :transaction-parent
           :make-transaction
           :database
           :make-database)
  ;; Lifecycle
  (:export :open-environment
           :close-environment
           :begin-transaction
           :commit-transaction
           :open-database
           :close-database)
  ;; Macros
  (:export :with-environment
           :with-database)
  ;; Methods
  (:export :version-string
           :environment-statistics
           :environment-info
           :cursor
           :make-cursor
           :get
           :put
           :del)
  (:documentation "The high-level LMDB interface."))
(in-package :lmdb)

;;; Library

(cffi:define-foreign-library liblmdb
  (:darwin (:or "liblmdb.dylib" "liblmdb.1.dylib"))
  (:unix  (:or "liblmdb.so" "liblmdb.so.0.0.0"))
  (:win32 "liblmdb.dll")
  (t (:default "liblmdb")))

(cffi:use-foreign-library liblmdb)

;;; Constants

(defparameter +permissions+ #o664
  "The Unix permissions to use for the database.")

;; Some error codes
(defparameter +enoent+ 2)
(defparameter +eacces+ 13)
(defparameter +eagain+ 11)

;;; Classes

(defclass environment ()
  ((handle :reader %handle
           :initarg :handle
           :documentation "The pointer to the environment handle.")
   (directory :reader environment-directory
              :initarg :directory
              :documentation "The directory where environment files are stored.")
   (max-databases :reader environment-max-dbs
                  :initarg :max-dbs
                  :initform 1
                  :type integer
                  :documentation "The maximum number of named databases."))
  (:documentation "Environment handle."))

(defclass transaction ()
  ((handle :reader %handle
           :initarg :handle
           :documentation "The pointer to the transaction.")
   (env :reader transaction-environment
        :initarg :environment
        :type environment
        :documentation "The environment this transaction belongs to.")
   (parent :reader transaction-parent
           :initarg :parent
           :type (or transaction null)
           :documentation "The parent transaction."))
  (:documentation "A transaction."))

(defclass database ()
  ((handle :reader %handle
           :initarg :handle
           :documentation "The DBI handle.")
   (transaction :reader database-transaction
                :initarg :transaction
                :type transaction
                :documentation "The transaction this database belongs to.")
   (name :reader database-name
         :initarg :name
         :type string
         :documentation "The database name.")
   (create :reader database-create-p
           :initarg :create
           :type boolean
           :documentation "Whether or not to create the database if it doesn't
           exist."))
  (:documentation "A database."))

(defstruct (value (:constructor %make-value))
  "A value is a generic representation of keys and values."
  (size 0 :type fixnum)
  data)

(defclass cursor ()
  ((handle :reader %handle
           :initarg :handle
           :documentation "The pointer to the cursor object.")
  ( database :reader cursor-database
             :initarg :database
             :type database
             :documentation "The database the cursor belongs to."))
  (:documentation "A cursor."))

;;; Constructors

(defun make-environment (directory &key (max-databases 1))
  "Create an environment object."
  (let ((instance (make-instance 'environment
                                 :handle (cffi:foreign-alloc :pointer)
                                 :directory directory
                                 :max-dbs max-databases)))
    (unless (= (lmdb.low:env-create (%handle instance))
               0)
      (error "Error creating environment object."))
    ;; Set the maximum number of databases
    (lmdb.low:env-set-maxdbs (handle instance)
                             (environment-max-dbs instance))
    instance))

(defun make-transaction (environment &optional parent)
  "Create a transaction object."
  (make-instance 'transaction
                 :handle (cffi:foreign-alloc :pointer)
                 :environment environment
                 :parent parent))

(defun make-database (transaction name
                      &key (create t))
  "Create a database object."
  (make-instance 'database
                 :handle (cffi:foreign-alloc :pointer)
                 :transaction transaction
                 :name name
                 :create create))

(defun convert-data (data)
  "Convert Lisp data to a format LMDB likes. Supported types are integers,
floats, booleans and strings. Returns a (size . array) pair."
  (typecase data
    (integer
     (let ((octets (bit-smasher:int->octets data)))
       (cons (length octets) octets)))
    (boolean
     (cons 1 (if data 1 0)))
    (string
     (let ((octets (trivial-utf-8:string-to-utf-8-bytes data)))
       (cons (length octets) octets)))
    ((vector (unsigned-byte 8))
     (cons (length data) data))
    (t
     (error "Invalid type."))))

(defun make-value (data)
  "Create a value object."
  (destructuring-bind (size . vector)
      (convert-data data)
    (%make-value :size size
                 :data vector)))

(defun make-cursor (transaction)
  "Create a cursor object."
  (make-instance 'cursor
                 :handle (cffi:foreign-alloc :pointer)
                 :transaction transaction))

;;; Errors

(define-condition lmdb-error ()
  ()
  (:documentation "The base class of all LMDB errors."))

;;; Viscera

(defun handle (object)
  "Return the handle from an environment, transaction or database."
  (cffi:mem-ref (%handle object) :pointer))

(defun open-environment (environment)
  "Open the environment connection."
  (with-slots (directory) environment
    (assert (uiop:directory-pathname-p directory))
    (ensure-directories-exist directory)
    (let ((return-code (lmdb.low:env-open (handle environment)
                                          (namestring directory)
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
         (error "Unknown error code: ~A" return-code)))))
  environment)

(defun environment-statistics (environment)
  "Return statistics about the environment."
  (cffi:with-foreign-object (stat '(:struct lmdb.low:stat))
    (lmdb.low:env-stat (handle environment)
                       stat)
    (macrolet ((slot (slot)
                 `(cffi:foreign-slot-value stat
                                           '(:struct lmdb.low:stat)
                                           ',slot)))
      (list :page-size (slot lmdb.low:ms-psize)))))

(defun environment-info (environment)
  "Return information about the environment."
  (cffi:with-foreign-object (info '(:struct lmdb.low:envinfo))
    (lmdb.low:env-info (handle environment)
                       info)
    (macrolet ((slot (slot)
                 `(cffi:foreign-slot-value info
                                           '(:struct lmdb.low:envinfo)
                                           ',slot)))
      (list :map-address (cffi:pointer-address (slot lmdb.low:me-mapaddr))
            :map-size (cffi:pointer-address (slot lmdb.low:me-mapsize))))))

(defun begin-transaction (transaction)
  "Begin the transaction."
  (with-slots (env parent) transaction
    (let ((return-code (lmdb.low:txn-begin (handle env)
                                           (if parent
                                               (handle parent)
                                               (cffi:null-pointer))
                                           0
                                           (%handle transaction))))
      (alexandria:switch (return-code)
        (0
         ;; Success
         )
        ;; TODO rest
        (t
         (error "Unknown error code."))))))

(defun commit-transaction (transaction)
  "Commit the transaction."
  (lmdb.low:txn-commit (handle transaction)))

(defun abort-transaction (transaction)
  "Abort the transaction."
  (lmdb.low:txn-abort (handle transaction)))

(defun open-database (database)
  "Open a database."
  (with-slots (transaction name create) database
    (let ((return-code (lmdb.low:dbi-open (handle transaction)
                                          name
                                          (logior 0
                                                  (if create
                                                      lmdb.low:+create+
                                                      0))
                                          (%handle database))))
      (alexandria:switch (return-code)
        (0
         ;; Success
         t)
        (lmdb.low:+notfound+
         (error "Database not found, and did not specify :create t."))
        (lmdb.low:+dbs-full+
         (error "Reached maximum number of named databases."))
        (t
         (error "Unknown error code: ~A" return-code)))))
  database)

(defun open-cursor (cursor)
  "Open a cursor."
  (with-slots (transaction) cursor
    (with-slots (database) transaction
      (let ((return-code (lmdb.low:cursor-open (handle transaction)
                                               (handle database)
                                               (%handle cursor))))
        (alexandria:switch (return-code)
          (0
           ;; Success
           t)
          (22
           (error "Invalid parameter."))
          (t
           (error "Unknown error code: ~A" return-code))))))
  cursor)

;;; Querying

(defmacro with-val ((raw-value data) &body body)
  (alexandria:with-gensyms (value-struct array)
    `(let* ((,value-struct (make-value ,data))
            (,raw-value (cffi:foreign-alloc '(:struct lmdb.low:val)))
            (,array (cffi:foreign-alloc :unsigned-char
                                        :count (value-size ,value-struct))))
       (setf (cffi:foreign-slot-value ,raw-value
                                      '(:struct lmdb.low:val)
                                      'lmdb.low:mv-size)
             (cffi:make-pointer (value-size ,value-struct)))

       (loop for elem across (value-data ,value-struct)
             for i from 0 to (1- (length (value-data ,value-struct)))
             do
                (setf (cffi:mem-aref ,array :unsigned-char i)
                      elem))
       (setf (cffi:foreign-slot-value ,raw-value
                                      '(:struct lmdb.low:val)
                                      'lmdb.low:mv-data)
             ,array)
       ,@body)))

(defmacro with-empty-value ((value) &body body)
  `(cffi:with-foreign-object (,value '(:struct lmdb.low:val))
     ,@body))

(defun raw-value-to-vector (raw-value)
  (let* ((size (cffi:pointer-address
                (cffi:foreign-slot-value raw-value
                                         '(:struct lmdb.low:val)
                                         'lmdb.low:mv-size)))
         (array (cffi:foreign-slot-value raw-value
                                         '(:struct lmdb.low:val)
                                         'lmdb.low:mv-data))
         (vec (make-array size
                          :element-type '(unsigned-byte 8))))
    (loop for i from 0 to (1- size) do
      (setf (elt vec i)
            (cffi:mem-aref array :unsigned-int i)))
    vec))

(defun get (database key)
  "Get a value from the database."
  (with-slots (transaction) database
    (with-val (raw-key key)
      (with-empty-value (raw-value)
        (let ((return-code (lmdb.low:get (handle transaction)
                                         (handle database)
                                         raw-key
                                         raw-value)))
          (alexandria:switch (return-code)
            (0
             ;; Success
             (raw-value-to-vector raw-value))
            (lmdb.low:+notfound+
             (error "Key not found: ~A." key))
            (t
             (error "Unknown error code."))))))))

(defun put (database key value)
  "Add a value to the database."
  (with-slots (transaction) database
    (with-val (raw-key key)
      (with-val (raw-val value)
        (let ((return-code (lmdb.low:put (handle transaction)
                                         (handle database)
                                         raw-key
                                         raw-val
                                         0)))
          (alexandria:switch (return-code)
            (0
             ;; Success
             )
            (t
             (error "Unknown error code: ~A" return-code)))))))
  value)

(defun del (database key &optional data)
  "Delete this key from the database."
  (with-slots (transaction) database
    (with-val (raw-key key)
      (with-empty-value (raw-value)
        (let ((return-code (lmdb.low:del (handle transaction)
                                         (handle database)
                                         raw-key
                                         (if data
                                             data
                                             (cffi:null-pointer)))))
          (alexandria:switch (return-code)
            (0
             ;; Success
             key)
            (+eacces+
             (error "An attempt was made to delete a key in a read-only transaction."))
            (t
             (error "Unknown error code."))))))))

;;; Destructors

(defun close-environment (environment)
  "Free the environment."
  (lmdb.low:env-close (handle environment))
  (cffi:foreign-free (%handle environment)))

(defun close-database (database)
  "Close the database."
  (lmdb.low:dbi-close (handle
                       (transaction-environment
                        (database-transaction database)))
                      (handle database))
  (cffi:foreign-free (%handle database)))

;;; Macros

(defmacro with-environment ((env) &body body)
  "Execute the body and close the environment."
  `(progn
     (open-environment ,env)
     (unwind-protect
          (progn ,@body)
       (close-environment ,env))))

(defmacro with-database ((database) &body body)
  "Execute the body and close the database."
  `(progn
     (open-database ,database)
     (unwind-protect
          (progn ,@body)
       (close-database ,database))))

;;; Utilities

(defun version-string ()
  "Return the version string."
  (format nil "~D.~D.~D"
          lmdb.low:+version-major+
          lmdb.low:+version-minor+
          lmdb.low:+version-patch+))
