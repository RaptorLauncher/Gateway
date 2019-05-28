;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; database/protocol/persistable.lisp

(in-package #:gateway.database/protocol)
!
(define-protocol persistable
    (:documentation "The PERSISTABLE protocol describes objects that are ~
persistable in a database and retrieved later on."
     :tags (:gateway :persistable)
     :dependencies ()
     :export t)
  (:class persistable () ())
  "A persistable object. See protocol PERSISTABLE for details.")

(execute-protocol persistable)
