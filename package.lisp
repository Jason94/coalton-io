(in-package  :cl-user)

(progn
  (ql:quickload :coalton))

; (defpackage :coalton-io
;     (:use #:coalton #:coalton-prelude)
;     (:local-nicknames (:ht #:coalton-library/hashtable)
;                       (:mp #:coalton-library/ord-map)
;                       (:state #:coalton-library/monad/state)
;                       (:str #:coalton-library/string))
;     (:import-from #:coalton-library/functions
;                   #:compose))
