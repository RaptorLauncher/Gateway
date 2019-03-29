;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/t/package.lisp

(uiop:define-package #:gateway.connector/test
  (:use #:cl
        #:alexandria
        #:phoe-toolbox
        #:named-readtables
        #:gateway.connector
        #:gateway.cable)
  (:import-from #:protest/test-case
                #:define-test-case)
  (:import-from #:protest/parachute
                #:define-test
                #:true #:false #:is #:isnt #:is-values #:isnt-values
                #:protest/parachute)
  (:export #:connector))

(in-package #:gateway.connector/test)

(define-test-case connector
    (:documentation "Test suite for Gateway connector."
     :tags (:gateway :connector :suite)))

(define-test connector
  :parent (#:gateway.init #:gateway-full-test))
