;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/t/package.lisp

(uiop:define-package #:gateway.connector/test
  (:use
   #:cl
   #:phoe-toolbox
   #:gateway.connector/protocol
   #:gateway.connector
   #:gateway.cable
   #:named-readtables)
  (:import-from
   #:protest/test-case
   #:define-test-case)
  (:import-from
   #:protest/parachute
   #:define-test
   #:true #:false #:is #:isnt #:is-values #:isnt-values
   #:protest/parachute))
