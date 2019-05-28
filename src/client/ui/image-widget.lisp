;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2019
;;;; image-widget.lisp

(in-package #:gateway.client.ui)
(in-readtable :qtools)

;;; TODO move to utils
(defmacro define-qt-constructor ((class . keys) &body body)
  `(define-constructor (,class ,@keys)
     (qtools:with-slots-bound (,class ,class)
       ,@body)))

(defun make-shadow-qpixmap (width height)
  (let* ((pixmap (q+:make-qpixmap width height)))
    (q+:fill pixmap (q+:make-qcolor 0 0 0 0))
    (with-finalizing ((gradient (q+:make-qlineargradient 0 0 0 height))
                      (painter (q+:make-qpainter pixmap)))
      (setf (q+:color-at gradient 0.0) (q+:qcolor-from-rgb 0 0 0 0)
            (q+:color-at gradient 1.0) (q+:qcolor-from-rgb 0 0 0 255))
      (q+:fill-rect painter 0 0 width height (q+:make-qbrush gradient)))
    pixmap))

(define-widget image-widget (qwidget)
  ((foreground-path :accessor foreground-path :initarg :foreground-path)
   (background-path :accessor background-path :initarg :background-path)
   (foreground :accessor foreground :initarg :foreground)
   (background :accessor background :initarg :background)
   (eye-level :accessor eye-level :initarg :eye-level)
   (min-width :accessor min-width :initarg :min-width)
   (shadow-height :accessor shadow-height :initarg :shadow-height)
   (background-hue :accessor background-hue :initarg :background-hue))
  (:default-initargs :foreground-path nil :foreground nil
                     :background-path nil :background nil
                     :eye-level nil :min-width nil :shadow-height 1000
                     :background-hue 0.0))

(define-subwidget (image-widget shadow)
    (make-shadow-qpixmap min-width shadow-height))

(define-constructor (image-widget foreground-path foreground
                                  background-path background)
  (cond ((and (null foreground) (null foreground-path))
         (error "Must specify FOREGROUND or FOREGROUND-PATH."))
        ((and foreground foreground-path)
         (error "Cannot specify both FOREGROUND and FOREGROUND-PATH."))
        (foreground-path
         (setf (foreground image-widget)
               (q+:make-qimage foreground-path))))
  (cond ((and (null background) (null background-path))
         (error "Must specify BACKGROUND or BACKGROUND-PATH."))
        ((and background background-path)
         (error "Cannot specify both BACKGROUND and BACKGROUND-PATH."))
        (background-path
         (setf (background image-widget)
               (q+:make-qimage background-path))))
  (when (/= 0.0 (background-hue image-widget))
    (qui:hue-shift (background image-widget) (background-hue image-widget)))
  (when (min-width image-widget)
    (setf (q+:minimum-width image-widget) (min-width image-widget))))

(define-override (image-widget paint-event) (ev)
  (with-finalizing ((painter (q+:make-qpainter image-widget)))
    (setf (q+:render-hint painter) (q+:qpainter.antialiasing))
    (q+:draw-tiled-pixmap painter (q+:rect image-widget)
                          (q+:qpixmap-from-image background))
    (let ((box (q+:rect image-widget))
          (height (q+:height image-widget))
          (width (q+:width image-widget))
          (foreground-height (q+:height foreground)))
      ;; TODO: draw shadow in a similar way as foreground
      (q+:draw-pixmap
       painter box shadow
       (q+:make-qrect 0 (- (q+:height shadow) height) width height))
      (let (result)
        (if (<= foreground-height height)
            (setf result (q+:make-qrect 0 (- foreground-height height)
                                        width (q+:height image-widget)))
            (let* ((ratio (/ height foreground-height))
                   (y (truncate (* foreground-height eye-level (- 1 ratio)))))
              (setf result (q+:make-qrect 0 y width height))))
        (q+:draw-image painter box foreground result)))))

(defun image1 ()
  (make-instance
   'image-widget
   :foreground-path "/home/phoe/Downloads/archie.png"
   :background-path "/home/phoe/Projects/Raptor Chat/tile2.png"
   :min-width 300 :shadow-height 1000
   :eye-level 0.115 :background-hue 1/6))

;; (defun image2 ()
;;   (make-instance
;;    'image-widget
;;    :width 150 :eye-level 74 :background-hue 80.0
;;    :background-path (homepath "tile.png")
;;    :shadow-path (homepath "shadow.png")
;;    :foreground-path (homepath "scaletail.png")))

;; (defun image ()
;;   (with-main-window (widget (q+:make-qwidget))
;;     (let ((layout (q+:make-qhboxlayout)))
;;       (setf (q+:layout widget) layout)
;;       (q+:add-widget layout (%image 0.0))
;;       (q+:add-widget layout (%image 60.0))
;;       (q+:add-widget layout (%image 120.0))
;;       (q+:add-widget layout (%image 180.0))
;;       (q+:add-widget layout (%image 240.0))
;;       (q+:add-widget layout (%image 300.0))
;;       (q+:resize widget 1 1000))))

;; (defun %image (&optional (hue 0.0))
;;   (make-instance
;;    'image-widget
;;    :width 150 :eye-level 74 :background-hue hue
;;    :background-path (homepath "tile2.png")
;;    :shadow-path (homepath "shadow.png")
;;    :foreground-path (homepath "erchembod.png")))
