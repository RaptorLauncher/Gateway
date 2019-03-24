;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2019
;;;; sql/t/package.lisp

(uiop:define-package #:gateway.sql/test
  (:use #:cl
        #:alexandria
        #:phoe-toolbox
        #:local-time
        #:named-readtables
        #:gateway.sql)
  (:import-from #:protest/test-case
                #:define-test-case)
  (:import-from #:protest/parachute
                #:true #:false #:is #:isnt #:is-values #:isnt-values
                #:protest/parachute)
  (:export #:sql))

(in-package #:gateway.sql/test)

(define-test-case sql
    (:documentation "Test suite for Gateway SQL layer."
     :tags (:gateway :sql :suite)))

(define-test sql)

