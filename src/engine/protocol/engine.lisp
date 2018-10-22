;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; engine/protocol/engine.lisp

(in-package #:gateway/protocol)

(define-protocol engine
    (:documentation "The ENGINE protocol describes objects which are capable ~
of processing messages, either sequentially or concurrently. In the second ~
case, the engine contains implementation-dependent objects called workers.
\
The message's format is implementation-dependent.
\
On each enqueued message, the engine's handler function is eventually called ~
on that message. That function is an one-argument function that expects the ~
implementation-dependent message as its argument."
     :tags (:gateway :engine)
     :dependencies (killable named with-handler connection message)
     :export t)
  (:class engine (killable named with-handler) ())
  "An engine object. See protocol ENGINE for details."
  (:function accept-message
             ((engine engine) (connection connection) (message message))
             (values))
  "Enqueues a message in the engine for processing with the provided ~
connection as the message author.")

(execute-protocol engine)
