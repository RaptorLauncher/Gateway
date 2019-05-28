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
   (eye-level :accessor eye-level :initarg :eye-level)
   (min-width :accessor min-width :initarg :min-width)
   (shadow-height :accessor shadow-height :initarg :shadow-height)
   (background-hue :accessor background-hue :initarg :background-hue))
  (:default-initargs :foreground-path (required-argument :foreground-path)
                     :background-path (required-argument :background-path)
                     :eye-level nil :min-width nil :shadow-height 1000
                     :background-hue 0.0))

(define-subwidget (image-widget foreground) (q+:make-qimage foreground-path))

(define-subwidget (image-widget shadow)
    (make-shadow-qpixmap min-width shadow-height))

(define-subwidget (image-widget background) (q+:make-qimage background-path))

(define-qt-constructor (image-widget)
  (when (/= 0.0 background-hue)
    (qcom:hue-shift background background-hue))
  (when min-width
    (setf (q+:minimum-width image-widget) min-width)))

(define-override (image-widget paint-event) (ev)
  (with-finalizing ((painter (q+:make-qpainter image-widget)))
    (setf (q+:render-hint painter) (q+:qpainter.antialiasing))
    (q+:draw-tiled-pixmap painter (q+:rect image-widget)
                          (q+:qpixmap-from-image background))
    (let ((box (q+:rect image-widget))
          (height (q+:height image-widget))
          (width (q+:width image-widget))
          (foreground-height (q+:height foreground))
          result)
      (q+:draw-pixmap
       painter box shadow
       (q+:make-qrect 0 (- (q+:height shadow) height) width height))
      (if (<= foreground-height height)
          (let ((widget-height (q+:height image-widget)))
            (setf result (q+:make-qrect 0 (- foreground-height height)
                                        width widget-height)))
          ;; TODO fix this
          (let* ((percentage (/ height foreground-height))
                 (y (round (+ (- (* percentage height))
                              (* eye-level foreground-height)))))
            (setf result (q+:make-qrect 0 y width height))))
      (q+:draw-image painter box foreground result))))

(defun image1 ()
  (make-instance
   'image-widget
   :foreground-path "/home/emiherd/Downloads/archie.png"
   :background-path "/home/emiherd/Projects/Raptor Chat/tile2.png"
   :min-width 300 :shadow-height 1000
   :eye-level 0.15 :background-hue 300.0))

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
