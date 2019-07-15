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

;; Examples

(defun image-widget-holder-example ()
  (with-main-window (window (make-instance 'image-widget-holder))
    (add-widget window (image2 "archie.png"   0.115  1  0.0))
    (add-widget window (image2 "cyan.png"     0.115  2  0.2))
    (add-widget window (image2 "scale.png"    0.18   5  0.0))
    (add-widget window (image2 "solyre.png"   0.07   5  0.0))
    (add-widget window (image2 "bahta.png"    0.17   3  0.0))
    (add-widget window (image2 "sashasa.png"  0.07   4  0.0))
    (add-widget window (image2 "tzix.png"     0.1    3  0.0))
    (q+:resize window 300 300)))
