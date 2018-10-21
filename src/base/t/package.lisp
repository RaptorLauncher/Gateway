;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; base/t/package.lisp

(uiop:define-package #:gateway.base/test
  (:mix #:list-named-class
        #:cl
        #:alexandria
        #:moptilities
        #:phoe-toolbox
        #:named-readtables
        #:gateway.cable
        #:gateway.base)
  (:import-from #:protest/base
                #:protocol-object-p)
  (:import-from #:protest/test-case
                #:define-test-case)
  (:import-from #:protest/parachute
                #:define-test
                #:true #:false #:is #:isnt #:is-values #:isnt-values
                #:protest/parachute)
  (:export #:base
           #:data-object
           #:*data-object-input*))

(in-package #:gateway.base/test)

(define-test-case base
    (:documentation "Test suite for Gateway base."
     :tags (:gateway :base :suite)))

(define-test base)
