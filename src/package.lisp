(in-package :cl-user)
(defpackage lmdb
  ;; Version info
  (:export :+version-major+
           :+version-minor+
           :+version-patch+
           :+version-full+
           :+version-date+
           :+version-string+)
  ;; Constants
  (:export :+notls+
           :+panic+
           :+append+
           :+create+
           :+nolock+
           :+nosync+
           :+rdonly+
           :+bad-dbi+
           :+bad-txn+
           :+current+
           :+dupsort+
           :+invalid+
           :+reserve+
           :+success+
           :+dbs-full+
           :+dupfixed+
           :+fixedmap+
           :+keyexist+
           :+map-full+
           :+mapasync+
           :+multiple+
           :+nosubdir+
           :+notfound+
           :+tls-full+
           :+txn-full+
           :+writemap+
           :+appenddup+
           :+bad-rslot+
           :+corrupted+
           :+nodupdata+
           :+nomeminit+
           :+nordahead+
           :+page-full+
           :+cp-compact+
           :+integerdup+
           :+integerkey+
           :+nometasync+
           :+reversedup+
           :+reversekey+
           :+bad-valsize+
           :+cursor-full+
           :+map-resized+
           :+nooverwrite+
           :+incompatible+
           :+last-errcode+
           :+readers-full+
           :+version-date+
           :+version-full+
           :+page-notfound+)
  (:documentation "The LMDB interface."))
