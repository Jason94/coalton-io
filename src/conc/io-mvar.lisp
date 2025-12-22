(cl:in-package :cl-user)

(defpackage :io/conc/mvar
  (:use
   #:io/io-impl/mvar
   #:io/gen-impl/conc/mvar)
  (:export
   ;; Re-exports from io/gen-impl/conc/mvar
   #:MVar
   #:new-mvar
   #:new-empty-mvar
   #:take-mvar
   #:put-mvar
   #:try-take-mvar
   #:try-put-mvar
   #:read-mvar
   #:try-read-mvar
   #:swap-mvar
   #:is-empty-mvar
   #:do-with-mvar

   #:MChan
   #:new-empty-chan
   #:push-chan
   #:pop-chan

   #:with-mvar

   ;; Re-exports from io/io-impl/mvar
   #:with-mvar_
   #:do-with-mvar_))

(in-package :io/conc/mvar)
