;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2019
;;;; image-widget.lisp

(in-package #:gateway.client.ui)
(in-readtable :qtools)

(define-widget image-widget (qwidget)
  ((foreground-path :accessor foreground-path :initarg :foreground-path)
   (background-path :accessor background-path :initarg :background-path)
   (foreground :accessor foreground :initarg :foreground)
   (background :accessor background :initarg :background)
   (eye-level :accessor eye-level :initarg :eye-level)
   (optimal-width :accessor optimal-width :initarg :optimal-width)
   (background-hue :accessor background-hue :initarg :background-hue))
  (:default-initargs :foreground-path nil :foreground nil
                     :background-path nil :background nil
                     :eye-level nil :optimal-width nil
                     :background-hue 0.0))

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

;;; Examples

(defun image1 (image eye-level tile-number hue)
  (make-instance
   'image-widget
   :foreground-path (homepath image)
   :background-path (homepath (format nil "tile~D.png" tile-number))
   :optimal-width 300 :eye-level eye-level :background-hue hue))

(with-main-window (window (make-instance 'image-widget-holder))
  (add-widget window (image1 "archie.png" 0.115 1 0.0))
  (add-widget window (image1 "cyan.png" 0.115 2 0.2))
  (add-widget window (image1 "scale.png" 0.18 5 0.0))
  (add-widget window (image1 "solyre.png" 0.07 5 0.0))
  (add-widget window (image1 "bahta.png" 0.17 3 0.0))
  (add-widget window (image1 "sashasa.png" 0.07 4 0.0))
  (add-widget window (image1 "tzix.png" 0.1 3 0.0))
  (q+:resize window 300 300))
