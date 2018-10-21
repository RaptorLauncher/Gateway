;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; base/protocol/identifiable.lisp

(in-package #:gateway.base/protocol)

(define-protocol identifiable
    (:documentation "The IDENTIFIABLE protocol describes objects which have ~
some sort of identifier by which they are recognized in a part of the system."
     :tags (:gateway :identifiable)
     :dependencies ()
     :export t)
  (:class identifiable () ())
  "An identifiable object. See protocol IDENTIFIABLE for details."
  (:function id ((identifiable identifiable)) t)
  "Returns the ID of the identifiable.")

(execute-protocol identifiable)
