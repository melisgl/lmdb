(in-package :cl-user)
(defpackage lmdb
  ;; Version info
  (:export :+mdb-version-major+
           :+mdb-version-minor+
           :+mdb-version-patch+
           :+mdb-version-full+
           :+mdb-version-date+
           :+mdb-version-string+)
  ;; Constants
  (:export :+mdb-notls+
           :+mdb-panic+
           :+mdb-append+
           :+mdb-create+
           :+mdb-nolock+
           :+mdb-nosync+
           :+mdb-rdonly+
           :+mdb-bad-dbi+
           :+mdb-bad-txn+
           :+mdb-current+
           :+mdb-dupsort+
           :+mdb-invalid+
           :+mdb-reserve+
           :+mdb-success+
           :+mdb-dbs-full+
           :+mdb-dupfixed+
           :+mdb-fixedmap+
           :+mdb-keyexist+
           :+mdb-map-full+
           :+mdb-mapasync+
           :+mdb-multiple+
           :+mdb-nosubdir+
           :+mdb-notfound+
           :+mdb-tls-full+
           :+mdb-txn-full+
           :+mdb-writemap+
           :+mdb-appenddup+
           :+mdb-bad-rslot+
           :+mdb-corrupted+
           :+mdb-nodupdata+
           :+mdb-nomeminit+
           :+mdb-nordahead+
           :+mdb-page-full+
           :+mdb-cp-compact+
           :+mdb-integerdup+
           :+mdb-integerkey+
           :+mdb-nometasync+
           :+mdb-reversedup+
           :+mdb-reversekey+
           :+mdb-bad-valsize+
           :+mdb-cursor-full+
           :+mdb-map-resized+
           :+mdb-nooverwrite+
           :+mdb-incompatible+
           :+mdb-last-errcode+
           :+mdb-readers-full+
           :+mdb-version-date+
           :+mdb-version-full+
           :+mdb-page-notfound+)
  (:documentation "The LMDB interface."))
