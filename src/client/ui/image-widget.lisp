;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2019
;;;; image-widget.lisp

(in-package #:gateway.client.ui)
(in-readtable :qtools)

(define-widget image-widget (qwidget)
  ((foreground :accessor foreground :initarg :foreground)
   (background :accessor background :initarg :background)
   (eye-level :accessor eye-level :initarg :eye-level)
   (optimal-width :accessor optimal-width :initarg :optimal-width)
   (background-hue :accessor background-hue :initarg :background-hue))
  (:default-initargs :foreground nil :background nil
                     :eye-level nil :optimal-width nil
                     :background-hue 0.0))

(defun make-shadow-qpixmap (width height)
  (let* ((pixmap (q+:make-qpixmap width height)))
    (q+:fill pixmap (q+:make-qcolor 0 0 0 0))
    (with-finalizing ((gradient (q+:make-qradialgradient (/ height 2)
                                                         (/ height 2)
                                                         (/ height 5/4)))
                      (painter (q+:make-qpainter pixmap)))
      (setf (q+:color-at gradient 0.0) (q+:qcolor-from-rgb 0 0 0 0)
            (q+:color-at gradient 1.0) (q+:qcolor-from-rgb 0 0 0 255))
      (q+:fill-rect painter 0 0 width height (q+:make-qbrush gradient)))
    pixmap))

(define-subwidget (image-widget shadow)
    (make-shadow-qpixmap optimal-width optimal-width))

(define-constructor (image-widget foreground-path foreground
                                  background-path background
                                  optimal-width)
  (macrolet ((set-layer (x x-path)
               `(cond ((and (null ,x) (null ,x-path))
                       (error "Must specify either ~A or ~A." ',x ',x-path))
                      ((and foreground foreground-path)
                       (error "Cannot specify both ~A and ~A." ',x ',x-path))
                      (,x-path
                       (setf (,x image-widget) (q+:make-qimage ,x-path))))))
    (set-layer foreground foreground-path)
    (set-layer background background-path))
  (when (/= 0.0 (background-hue image-widget))
    (qui:hue-shift (background image-widget) (background-hue image-widget)))
  (when optimal-width
    (q+:resize image-widget optimal-width 100))
  (setf (q+:minimum-width image-widget) 1
        (q+:minimum-height image-widget) 1))

;; TODO refactor
(define-override (image-widget paint-event) (ev)
  (let ((box (q+:rect image-widget))
        (height (q+:height image-widget))
        (width (q+:width image-widget))
        (bg-width (q+:width background)))
    (flet ((scale (image &optional (multiplier 1) (width width))
             (q+:scaled-to-width image (round (* width multiplier))
                                 (q+:qt.smooth-transformation))))
      (with-finalizing
          ((painter (q+:make-qpainter image-widget))
           (scaled-foreground (scale foreground)))
        (setf (q+:render-hint painter) (q+:qpainter.antialiasing))
        (let ((foreground-height (q+:height scaled-foreground))
              (ratio (float (/ (q+:width scaled-foreground)
                               (q+:width foreground)))))
          (with-finalizing*
              ((background (q+:qpixmap-from-image background))
               (scaled-background (scale background ratio bg-width)))
            (q+:draw-tiled-pixmap painter (q+:rect image-widget)
                                  scaled-background))
          (q+:draw-pixmap painter box shadow (q+:rect shadow))
          (q+:draw-image painter box scaled-foreground
                         (if (<= foreground-height height)
                             (q+:make-qrect 0 (- foreground-height height)
                                            width (q+:height image-widget))
                             (let* ((ratio (/ height foreground-height))
                                    (y (truncate (* foreground-height eye-level
                                                    (- 1 ratio)))))
                               (q+:make-qrect 0 y width height)))))))))

(define-finalizer (image-widget finalize-image-widget)
  (when foreground (finalize foreground))
  (when background (finalize background)))

(defun make-image-widget (image)
  (make-instance 'image-widget
                 :foreground-path (foreground-path image)
                 :background-path (background-path image)
                 :eye-level (eye-level image)
                 :optimal-width (optimal-width image)
                 :background-hue (background-hue image)))
