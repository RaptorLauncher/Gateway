;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; engine/package.lisp

(uiop:define-package #:gateway.engine
  (:use #:gateway.engine/protocol
        #:gateway.engine/condition
        #:gateway.engine/impl)
  (:reexport #:gateway.engine/protocol
             #:gateway.engine/condition
             #:gateway.engine/impl))

;; TODO Class SENTINEL
;; - AUTHENTICATION authenticator connection -> T/account/NIL
;; - (SETF AUTHENTICATION) new-value authenticator connection
;; - AUTHORIZE authenticator connection command -> ()/!?
;; - ENCRYPT authenticator connection command -> string/!?
;; - DECRYPT authenticator connection string -> command/!?

;; TODO take care of SERIALIZABLE protocol, somehow
;; TODO Gateway protocol on top of cable
