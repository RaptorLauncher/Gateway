;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/protocol/writer.lisp

(in-package #:gateway.connector/protocol)

(define-protocol writer
    (:documentation "The WRITER protocol describes objects which are ~
responsible for writing data to connections. Only writers are allowed to write ~
to connections to avoid race conditions."
     :tags (:gateway :writer)
     :dependencies (connection killable named)
     :export t)
  (:class writer (killable named handling) ())
  "A writer object. See protocol WRITER for details."
  (:function write-data ((writer writer) (connection connection) data) t)
  "Schedules the data to be written by the writer to the connection.")

(execute-protocol writer)
