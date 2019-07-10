;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; definition.lisp

(in-package #:gateway.client.ui)
(in-readtable :qtools)

(define-widget raptor-chat (qwidget) ())

(define-subwidget (raptor-chat layout) (q+ make-qhboxlayout raptor-chat)
  (setf (q+:margin layout) 0))
