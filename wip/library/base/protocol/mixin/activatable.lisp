;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; base/protocol/activatable.lisp

(in-package #:gateway.base/protocol)

(define-protocol activatable
    (:documentation "The ACTIVATABLE protocol describes objects which may be ~
active or inactive and have mechanisms for querying and setting that state."
     :tags (:gateway :activatable)
     :dependencies ()
     :export t)
  (:class activatable () ())
  "An activatable object. See protocol ACTIVATABLE for details."
  (:function activatedp ((activatable activatable)) boolean)
  "Returns true if the activatable is activated, false otherwise."
  (:function (setf activatedp) ((new-value boolean) (activatable activatable))
             boolean)
  "Sets the activation status of the activatable.")

(execute-protocol activatable)
