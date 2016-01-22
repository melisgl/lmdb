(defsystem lmdb-low
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (:cffi)
  :components ((:module "src"
                :components
                ((:module "low"
                  :serial t
                  :components
                  ((:file "package")
                   (:file "lispify")
                   (:file "wrapper"))))))
  :description "Low-level CFFI bindings to LMDB.")
