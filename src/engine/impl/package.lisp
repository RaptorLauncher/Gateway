;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/impl/package.lisp

(uiop:define-package #:gateway.engine/impl
  (:use #:cl
        #:alexandria
        #:phoe-toolbox
        #:moptilities
        #:protest/base
        #:gateway.cable
        #:gateway.engine/protocol
        #:gateway.engine/condition)
  (:import-from #:destructuring-bind-star
                #:destructuring-error
                #:destructuring-error-lambda-list
                #:destructuring-error-expression
                #:destructuring-error-reason)
  (:shadow #:destructuring-bind)
  (:export #:standard-message))

(in-package #:gateway.engine/impl)

(setf (macro-function 'destructuring-bind)
      (macro-function 'destructuring-bind-star:destructuring-bind*))
