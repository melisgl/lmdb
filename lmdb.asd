;;;; -*-mode: Lisp; coding: utf-8;-*-

(asdf:defsystem :lmdb
  :author "Fernando Borretti <eudoxiahp@gmail.com>, James Anderson <james.anderson@setf.de>, Gábor Melis <mega@retes.hu>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT, see COPYING."
  :version "0.1"
  :homepage "https://github.com/antimer/lmdb"
  :bug-tracker "https://github.com/antimer/lmdb/issues"
  :source-control (:git "git@github.com:antimer/lmdb.git")
  :depends-on (#:alexandria #:trivial-utf-8 #:cl-reexport #:mgl-pax
                            #:bordeaux-threads #:osicat
                            #:trivial-features #:trivial-garbage)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "liblmdb")
                             (:file "lmdb")
                             (:file "lmdb+"))))
  :description "Bindings to LMDB, the Lightning Memory-mapped Database."
  :in-order-to ((asdf:test-op (asdf:test-op "lmdb/test"))))

(asdf:defsystem :lmdb/test
  :licence "MIT, see COPYING."
  :version "0.1"
  :depends-on (#:lmdb #:try)
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "test-lmdb"))))
  :perform (asdf:test-op (o s)
             (uiop:symbol-call '#:lmdb/test '#:test)))
