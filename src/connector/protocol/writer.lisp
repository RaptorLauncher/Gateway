;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/protocol/writer.lisp

(in-package #:gateway.connector/protocol)

(define-protocol writer
    (:documentation "The WRITER protocol describes objects which are ~
responsible for writing data to connections. Only writers are allowed to write ~
to connections to avoid race conditions."
     :tags (:gateway :engine :writer)
     :dependencies (connection killable named writing)
     :export t)
  (:class writer (killable named writing) ())
  "A writer object. See protocol WRITER for details.")

(execute-protocol writer)
