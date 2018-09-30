;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; engine/condition/package.lisp

(uiop:define-package #:gateway.engine/condition
  (:use #:cl
        #:alexandria
        #:moptilities
        #:gateway.engine/protocol)
  (:import-from #:destructuring-bind-star
                #:destructuring-error
                #:lambda-list #:expression #:reason)
  (:shadow #:destructuring-bind)
  (:export #:gateway-destructuring-error #:expression #:reason
           #:message-read-error
           #:invalid-message-id
           #:invalid-message-type
           #:invalid-message-body))

(in-package #:gateway.engine/condition)

(setf (macro-function 'destructuring-bind)
      (macro-function 'destructuring-bind-star:destructuring-bind*))
