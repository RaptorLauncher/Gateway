;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/package.lisp

(uiop:define-package #:gateway.connector
  (:use #:gateway.connector/protocol
        #:gateway.connector/impl)
  (:reexport #:gateway.connector/protocol
             #:gateway.connector/impl))
