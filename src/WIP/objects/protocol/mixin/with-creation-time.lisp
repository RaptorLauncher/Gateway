;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; objects/protocol/with-creation-time.lisp

(in-package #:gateway.base/protocol)

(define-protocol with-creation-time
    (:documentation "The WITH-CREATION-TIME protocol describes objects which
have some sort of identifier by which they are recognized in a part of the
system."
     :tags (:gateway :with-creation-time)
     :dependencies ()
     :export t)
  (:class with-creation-time () ())
  "An object with creation time. See protocol WITH-CREATION-TIME for details."
  ;; TODO actually put creation time stuff here
  ;; (:function creation-time ((object with-creation-time)) t)
  ;; TODO date protocol
  ;; "Returns the creation time of the object"
  )

(execute-protocol with-creation-time)
