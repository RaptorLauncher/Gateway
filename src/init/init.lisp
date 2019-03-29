;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; init/init.lisp

(uiop:define-package #:gateway.init
  (:use #:cl
        #:protest/parachute)
  (:import-from #:protest/test-case
                #:define-test-case)
  (:export #:gateway-full-test))

(in-package #:gateway.init)

(define-test-case gateway-full-test
    (:documentation "The full test suite for Gateway.
\
A test database connection is required in order to run this test suite."
     :tags (:gateway :suite :full)))

(define-test gateway-full-test)

