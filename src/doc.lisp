(in-package :lmdb)

;;;; Register in PAX World

(defun pax-sections ()
  (list @lmdb-manual))
(defun pax-pages ()
  `((:objects
     (, @lmdb-manual)
     :source-uri-fn ,(make-github-source-uri-fn
                      :lmdb
                      "https://github.com/melisgl/lmdb"))))
(register-doc-in-pax-world :lmdb (pax-sections) (pax-pages))

#+nil
(progn
  (update-asdf-system-readmes @lmdb-manual :lmdb)
  (update-asdf-system-html-docs @lmdb-manual :lmdb
                                :pages (pax-pages)))
