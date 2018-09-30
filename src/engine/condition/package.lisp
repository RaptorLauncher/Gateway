;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; engine/condition/package.lisp

(defpackage #:gateway.engine/condition
  (:use #:cl
        #:gateway.engine/protocol)
  (:export #:destructuring-error
           #:message-read-error
           #:invalid-message-id
           #:invalid-message-type
           #:invalid-message-body))
