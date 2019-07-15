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

(defparameter *image-widget-holder-example-data*
  '(("archie.png"   0.115  1  0.0)
    ("cyan.png"     0.115  2  0.2)
    ("scale.png"    0.18   5  0.0)
    ("solyre.png"   0.07   5  0.0)
    ("bahta.png"    0.17   3  0.0)
    ("sashasa.png"  0.07   4  0.0)
    ("tzix.png"     0.1    3  0.0)))

(defun image-widget-holder-example ()
  (flet ((make (foreground-path eye-level tile-number background-hue)
           (make-image-widget
            (make-instance
             'image
             :foreground-path (homepath foreground-path)
             :background-path (homepath (format nil "tile~D.png" tile-number))
             :optimal-width 300
             :eye-level eye-level
             :background-hue background-hue))))
    (with-main-window (window (make-instance 'image-widget-holder))
      (dolist (image-data *image-widget-holder-example-data*)
        (add-widget window (apply #'make image-data)))
      (q+:resize window 300 600))))
