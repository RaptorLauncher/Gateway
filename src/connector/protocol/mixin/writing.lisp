;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/protocol/mixin/writing.lisp

(in-package #:gateway.connector/protocol)

(define-protocol writing
    (:documentation "The WRITING protocol describes objects which are ~
capable of writing data to connections.."
     :tags (:gateway :writing)
     :dependencies (connection)
     :export t)
  (:class writing () ())
  "A writing object. See protocol WRITING for details."
  (:function write-data ((writing writing) (connection connection) data) t)
  "Schedules the data to be written by the writing object to the connection.")

(execute-protocol writing)
