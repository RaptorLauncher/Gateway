;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017-2019
;;;; image-widget-holder.lisp

(in-package :gateway.client.ui)
(in-readtable :qtools)

(define-widget image-widget-holder (qwidget)
  ((widgets :reader widgets :initform '())))

;; TODO everywhere: pass the widget as the first argument to the layout
;; constructor so you don't have to setf the layout.
(define-subwidget (image-widget-holder layout)
    (q+:make-qvboxlayout image-widget-holder)
  (setf (q+:contents-margins layout) (values 0 0 0 0)))

(define-constructor (image-widget-holder widgets)
  (dolist (widget widgets)
    (add-widget image-widget-holder widget)))

(defmethod add-widget (image-widget-holder widget)
  (with-slots-bound (image-widget-holder image-widget-holder)
    (push widget widgets)
    (q+:add-widget layout widget)))
