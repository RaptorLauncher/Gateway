;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; cable/package.lisp

(defpackage #:gateway.cable
  (:use #:cl)
  (:export
   ;; BUFFER
   #:clean-buffers
   ;; INPUT/OUTPUT
   #:to-cable
   #:from-cable
   #:from-cable-buffered
   #:incomplete-input
   #:cable-error
   #:read-limit-hit
   #:*read-limit*
   ;; EQUAL
   #:cable-equal))
