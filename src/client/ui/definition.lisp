;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; definition.lisp

(in-package #:gateway.client.ui)
(in-readtable :qtools)

(define-widget raptor-chat (qwidget) ())

(define-subwidget (raptor-chat layout) (q+ make-qhboxlayout)
  (setf (q+:layout raptor-chat) layout
        (q+:margin layout) 0))
