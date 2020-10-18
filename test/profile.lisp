(in-package :lmdb/test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-sprof))

(defun test-string-perf ()
  (progn;sb-sprof:with-profiling (:report :graph)
    (with-temporary-env (env)
      (let ((db (get-db "db" :if-does-not-exist :create
                        :value-encoding :utf-8)))
        (declare (optimize speed))
        (let ((k "lkjdslkfdsaafiupoewiu324kjasdflkjadsfsaf")
              (v "lkjdsafiupoewiru324kjasdflkjadsfsaf"))
          (with-txn (:write t)
            (put db k v))
          (time
           (loop repeat 10000000 do
             (with-txn ()
               (g3t db k)))))))))

#+nil
(progn
  (sb-ext:gc :full t)
  (test-string-perf))

(defun test-octets-write-perf ()
  (with-temporary-env (env :sync t :meta-sync nil)
    (let ((db (get-db "db" :if-does-not-exist :create)))
      (declare (optimize speed))
      (progn                 ;sb-sprof:with-profiling (:report :graph)
        (let ((k (make-array 10 :element-type 'lmdb::octet))
              (v (make-array 40 :element-type 'lmdb::octet)))
          (time
           (loop repeat 1000 do
             (with-txn (:write t)
               (put db k v)))))))))

#+nil
(test-octets-write-perf)

(defun test-octets-perf ()
  (with-temporary-env (env)
    (let ((db (get-db "db" :if-does-not-exist :create)))
      (declare (optimize speed))
      (let ((k (make-array 100 :element-type 'lmdb::octet))
            (v (make-array 4000 :element-type 'lmdb::octet)))
        (with-txn (:write t)
          (put db k v))
        (progn;sb-sprof:with-profiling (:report :graph)
          (time
           (loop repeat 10000000 do
             (with-txn ()
               (g3t db k)))))))))

#+nil
(let ((lmdb::*value-decoder* (constantly nil)))
  (sb-ext:gc :full t)
  (test-octets-perf))

(defun test-cursor-perf ()
  (with-temporary-env (env)
    (let ((db (get-db "db" :if-does-not-exist :create)))
      (declare (optimize speed))
      (let ((k (make-array 100 :element-type 'lmdb::octet))
            (v (make-array 4000 :element-type 'lmdb::octet)))
        (with-txn (:write t)
          (put db k v))
        (progn;sb-sprof:with-profiling (:report :graph)
          (time
           (loop repeat 10000000 do
             (with-txn ()
               (with-cursor (cur db)
                 (cursor-set-key k cur))))))))))

#+nil
(let ((lmdb::*value-decoder* (constantly nil)))
  (sb-ext:gc :full t)
  (test-cursor-perf))

(defun test-implicit-cursor-perf ()
  (with-temporary-env (env)
    (let ((db (get-db "db" :if-does-not-exist :create)))
      (declare (optimize speed))
      (let ((k (make-array 100 :element-type 'lmdb::octet))
            (v (make-array 4000 :element-type 'lmdb::octet)))
        (with-txn (:write t)
          (put db k v))
        (progn;sb-sprof:with-profiling (:report :graph)
          (time
           (loop repeat 10000000 do
             (with-txn ()
               (with-implicit-cursor (db)
                 (cursor-set-key k))))))))))

#+nil
(let ((lmdb::*value-decoder* (constantly nil)))
  (sb-ext:gc :full t)
  (test-implicit-cursor-perf))


;;; 822 bytes when encoded
(defun make-xxx ()
  (let ((x (list "235" 7 3.5 (vector :xxx34234 'lmdb:g3t)
                 (find-package :lmdb) (make-hash-table)
                 (cons 5/7 -3))))
    (append x x x x x x x x x x x x x)))

#+nil
(time
 (let ((xxx (make-xxx)))
   (loop repeat 100000 do (cpk:encode xxx))))

#+nil
(time
 (let ((xxx (cpk:encode (make-xxx))))
   (loop repeat 100000 do (cpk:decode xxx))))

#+nil
(defun test-manual-cpk-perf ()
  (sb-sprof:with-profiling (:report :graph)
    (with-temporary-env (env)
      (let ((db (get-db "db" :if-does-not-exist :create)))
        (declare (optimize speed))
        (let ((k 1)
              (v (cpk:encode (make-xxx))))
          (with-txn (:write t)
            (put db k v))
          (loop repeat 100000 do
            (with-txn ()
              (cpk:decode (g3t db k)))))))))

#+nil
(progn
  (sb-ext:gc :full t)
  (time (test-manual-cpk-perf)))

#+nil
(loop for i upfrom 0
      do (when (zerop (mod i 10))
           (format t "~S~%" i)
           (sb-ext:gc :full t))
         (test))
