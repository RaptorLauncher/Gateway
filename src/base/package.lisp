;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; base/package.lisp

(uiop:define-package #:gateway.base
  (:use #:gateway.base/protocol
        #:gateway.base/impl)
  (:reexport #:gateway.base/protocol
             #:gateway.base/impl))

;; TODO Class SENTINEL
;; - AUTHENTICATION authenticator connection -> T/account/NIL
;; - (SETF AUTHENTICATION) new-value authenticator connection
;; - AUTHORIZE authenticator connection command -> ()/!?
;; - ENCRYPT authenticator connection command -> string/!?
;; - DECRYPT authenticator connection string -> command/!?
