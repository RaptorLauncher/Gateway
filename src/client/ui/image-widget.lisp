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
   (shadow-level :accessor shadow-level :initarg :shadow-level)
   (shadow-height :accessor shadow-height :initarg :shadow-height)
   (background-hue :accessor background-hue :initarg :background-hue))
  (:default-initargs :foreground-path nil :foreground nil
                     :background-path nil :background nil
                     :eye-level nil :optimal-width nil
                     :shadow-level 0.5 :shadow-height 1000
                     :background-hue 0.0))

;; TODO move to utils
(defun make-shadow-qpixmap (width height)
  (let* ((pixmap (q+:make-qpixmap width height)))
    (q+:fill pixmap (q+:make-qcolor 0 0 0 0))
    (with-finalizing ((gradient (q+:make-qlineargradient 0 0 0 height))
                      (painter (q+:make-qpainter pixmap)))
      (setf (q+:color-at gradient 0.0) (q+:qcolor-from-rgb 0 0 0 0)
            (q+:color-at gradient 1.0) (q+:qcolor-from-rgb 0 0 0 255))
      (q+:fill-rect painter 0 0 width height (q+:make-qbrush gradient)))
    pixmap))

(define-subwidget (image-widget shadow)
    (make-shadow-qpixmap optimal-width shadow-height))

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
  (with-finalizing ((painter (q+:make-qpainter image-widget)))
    (setf (q+:render-hint painter) (q+:qpainter.antialiasing))
    (let ((box (q+:rect image-widget))
          (height (q+:height image-widget))
          (width (q+:width image-widget))
          (bg-width (q+:width background)))
      (flet ((scale (image &optional (multiplier 1) (width width))
               (q+:scaled-to-width image (round (* width multiplier))
                                   (q+:qt.smooth-transformation))))
        (with-finalizing
            ((scaled-shadow (scale shadow))
             (scaled-foreground (scale foreground)))
          (let ((foreground-height (q+:height scaled-foreground))
                (ratio (float (/ (q+:width scaled-foreground)
                                 (q+:width foreground)))))
            (with-finalizing*
                ((background (q+:qpixmap-from-image background))
                 (scaled-background (scale background ratio bg-width)))
              (q+:draw-tiled-pixmap painter (q+:rect image-widget)
                                    scaled-background))
            (flet ((variable-height-box (value)
                     (let* ((ratio (/ height foreground-height))
                            (y (truncate (* foreground-height value
                                            (- 1 ratio)))))
                       (q+:make-qrect 0 y width height))))
              (q+:draw-pixmap painter box scaled-shadow
                              (if (<= foreground-height height)
                                  (q+:make-qrect
                                   0 (- (q+:height scaled-shadow) height)
                                   width height)
                                  (variable-height-box shadow-level)))
              (q+:draw-image painter box scaled-foreground
                             (if (<= foreground-height height)
                                 (q+:make-qrect 0 (- foreground-height height)
                                                width (q+:height image-widget))
                                 (variable-height-box eye-level))))))))))

(define-finalizer (image-widget finalize-image-widget)
  (when foreground (finalize foreground))
  (when background (finalize background)))

;;; Examples

(defun image1 ()
  (make-instance
   'image-widget
   :foreground-path (homepath "archie.png")
   :background-path (homepath "tile2.png")
   :optimal-width 300 :eye-level 0.115 :background-hue (random 1.0)
   :shadow-level 0.2 :shadow-height 1000))

;;; TODO move to utils or remove
;; (defmacro define-qt-constructor ((class . keys) &body body)
;;   `(define-constructor (,class ,@keys)
;;      (qtools:with-slots-bound (,class ,class)
;;        ,@body)))
