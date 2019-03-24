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
                #:true #:false #:is #:isnt #:is-values #:isnt-values :fail
                #:protest/parachute)
  (:export #:sql #:sql-positive #:sql-negative))
