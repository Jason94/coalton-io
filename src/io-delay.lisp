(cl:in-package :cl-user)

(defpackage :io/delay
  (:use
   #:io/classes/delay
   #:io/io-impl/io-runtime/delay
   #:io/io-impl/delay)
  (:export
   ;; Re-exports from io/classes/delay
   #:DelayRuntime
   #:fork-delay!

   #:Delays
   #:fork-delay
   #:do-fork-delay

   ;; Re-exports from io/io-impl/io-runtime/delay
   #:IoDelay

   ;; Re-exports from io/io-impl/delay
   #:fork-delay_
   #:do-fork-delay_
   ))
