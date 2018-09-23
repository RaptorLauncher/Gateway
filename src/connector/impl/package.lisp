;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/impl/package.lisp

(uiop:define-package #:gateway.connector
  (:use
   #:cl
   #:alexandria
   #:phoe-toolbox
   #:gateway.connector/protocol
   #:gateway.cable)
  (:reexport
   #:gateway.connector/protocol)
  (:export
   ;; UTILS
   #:socket-local-address
   #:socket-peer-address
   #:make-connection-pair
   ;; CLASSES
   #:standard-connection
   #:standard-acceptor
   #:standard-listener
   #:standard-writer
   #:standard-connector))
