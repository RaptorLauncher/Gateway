;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; gateway.connector/impl/package.lisp

(uiop:define-package #:gateway.connector
  (:use
   #:cl
   #:gateway.connector/protocol)
  (:reexport
   #:gateway.connector/protocol)
  (:export
   #:standard-connection
   #:standard-acceptor
   #:standard-listener
   #:standard-writer
   #:standard-connector))
