;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; engine/t/package.lisp

(uiop:define-package #:gateway.engine/test
  (:use #:cl
        #:alexandria
        #:moptilities
        #:phoe-toolbox
        #:named-readtables
        #:gateway.cable
        #:gateway.engine)
  (:import-from #:protest/base
                #:protocol-object-p)
  (:import-from #:protest/test-case
                #:define-test-case)
  (:import-from #:protest/parachute
                #:define-test
                #:true #:false #:is #:isnt #:is-values #:isnt-values
                #:protest/parachute)
  (:export #:engine))

(in-package #:gateway.engine/test)

(define-test-case engine
    (:documentation "Test suite for Gateway engine."
     :tags (:gateway :engine :suite)))

(define-test engine)
