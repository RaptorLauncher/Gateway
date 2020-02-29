;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; engine/package.lisp

(uiop:define-package #:gateway.engine
  (:use #:gateway.engine/protocol
        #:gateway.engine/impl)
  (:reexport #:gateway.engine/protocol
             #:gateway.engine/impl))

;; TODO Class SENTINEL
;; - AUTHENTICATION connection sentinel -> T/account/NIL
;; - (SETF AUTHENTICATION) new-value connection sentinel
;; - AUTHORIZE sentinel connection command -> ()/!?
;; - ENCRYPT sentinel connection command -> string/!?
;; - DECRYPT sentinel connection string -> command/!?
