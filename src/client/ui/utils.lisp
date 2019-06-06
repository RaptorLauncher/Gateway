;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017-2019
;;;; utils.lisp

(in-package :gateway.client.ui)
(in-readtable :qtools)

(defun homepath (filename)
  (uiop:native-namestring
   (uiop:nest
    (merge-pathnames filename)
    (merge-pathnames "Projects/Raptor Chat/")
    (user-homedir-pathname))))

;;; PLACEHOLDER-TEXT-EDIT

(defun make-placeholder-text-edit (placeholder)
  (make-instance 'qui:placeholder-text-edit :placeholder placeholder))

;;; PLACECHCKED-TEXT-EDIT

(define-widget placechecked-text-edit (qtextedit qui:spellchecked-text-edit
                                                 qui:placeholder-text-edit)
  ())

(defun make-placechecked-text-edit (placeholder)
  (make-instance 'placechecked-text-edit :placeholder placeholder))

;;; MAKE-SHADOW-QPIXMAP

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

;;; DEFINE-QT-CONSTRUCTOR

;; TODO remove if unused
(defmacro define-qt-constructor ((class . keys) &body body)
  `(define-constructor (,class ,@keys)
     (qtools:with-slots-bound (,class ,class)
       ,@body)))
