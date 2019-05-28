;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; base/impl/package.lisp

(uiop:define-package #:gateway.base/impl
  (:mix #:list-named-class
        #:cl
        #:alexandria
        #:phoe-toolbox
        #:moptilities
        #:protest/base
        #:gateway.cable
        #:gateway.base/protocol)
  (:import-from #:destructuring-bind-star
                #:destructuring-error #:expression #:reason
                #:destructuring-error-lambda-list
                #:destructuring-error-expression
                #:destructuring-error-reason)
  (:shadow #:destructuring-bind)
  (:export #:read-error #:expression #:reason
           #:object-read-error
           #:message-read-error
           #:invalid-message-id
           #:invalid-message-class
           #:invalid-message-body)
  (:export #:standard-message))

(in-package #:gateway.base/impl)

(setf (macro-function 'destructuring-bind)
      (macro-function 'destructuring-bind-star:destructuring-bind*))
