;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; database/protocol/database.lisp

(in-package #:gateway.database/protocol)

(define-protocol database
    (:documentation "The DATABASE protocol describes representing databases ~
capable of storing and retrieving persistable objects.
\
The generic functions for accessing databases should be defined in separate ~
protocols depending on this protocol."
     :tags (:gateway :database)
     :dependencies (killable persistable) ;; TODO persistable
     :export t)
  (:class database (killable) ())
  "An database object. See protocol DATABASE for details."
  (:function persist ((database database) (persistable persistable)) (values))
  "Persists the provided object in the database.")

(execute-protocol database)
