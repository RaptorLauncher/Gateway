;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/impl/package.lisp

(uiop:define-package #:gateway.connector/impl
  (:use
   #:cl
   #:alexandria
   #:phoe-toolbox
   #:gateway.connector/protocol
   #:gateway.cable)
  (:export
   ;; UTILS
   #:socket-local-address
   #:socket-peer-address
   #:server-socket-alive-p
   #:make-connection-pair
   #:wait-for-sockets
   ;; CLASSES
   #:standard-connection
   #:standard-acceptor
   #:standard-listener
   #:standard-writer
   #:standard-connector))
