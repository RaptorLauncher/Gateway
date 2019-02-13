(ql:quickload '(lorem-ipsum qtools qtcore qtgui qtsvg))
(named-readtables:in-readtable :qtools)
(in-package #:cl+qt)

;;; FIXED-QTEXTEDIT

(define-widget fixed-qtextedit (qtextedit) ())

(define-subwidget (fixed-qtextedit fix-context-menu-widget) (q+:make-qwidget))

(define-override (fixed-qtextedit context-menu-event) (event)
  ;; Print the number of the clicked block
  #+(or)
  (let* ((position (q+:pos event))
         (cursor (q+:cursor-for-position fixed-qtextedit position))
         (block-number (q+:block-number cursor)))
    (print block-number)
    (call-next-qmethod))
  (let ((position (q+:pos event)))
    ;; I have no idea why it works this way, but it seems to work.!
    (call-next-qmethod)
    ;; Fix cursor position after the context menu disappears.
    (let* ((cursor (q+:cursor-for-position fixed-qtextedit position)))
      (setf (q+:text-cursor fixed-qtextedit) cursor))
    ;; Display the context menu.
    (with-finalizing ((menu (q+:create-standard-context-menu fixed-qtextedit)))
      (q+:exec menu (q+:global-pos event)))
    ;; Work around the bug.
    (q+:show fix-context-menu-widget)
    (setf (q+:focus fix-context-menu-widget) 0)
    (q+:hide fix-context-menu-widget)))

;;; POST

(defclass post ()
  ((%player :accessor player :initarg :player)
   (%persona :accessor persona :initarg :persona)
   (%contents :accessor contents :initarg :contents)
   (%timestamp :accessor timestamp :initarg :timestap)))

;;; Test

(defparameter *page-divider-pathname*
  (asdf:system-relative-pathname
   :gateway.ui.assets #p"Page-Dividers/*.svg"))

(defparameter *page-divider-namestring*
  (uiop:native-namestring
   (merge-pathnames "webdesignhot-1-1.svg" *page-divider-pathname*)))

(defun test ()
  (with-main-window (editor 'fixed-qtextedit)
    (with-finalizing ((renderer (q+:make-qsvgrenderer)))
      (let ((prologue t))
        (dolist (pathname (directory *page-divider-pathname*))
          (append-at editor :justify
                     (lorem-ipsum:paragraph :prologue prologue))
          (setf prologue nil)
          (let ((url (uiop:strcat "gateway://" (pathname-name pathname)))
                (document (q+:document editor)))
            (with-finalizing
                ((pixmap (svg-pixmap pathname :renderer renderer)))
              (q+:add-resource document (q+:qtextdocument.image-resource)
                               (q+:make-qurl url) (q+:to-image pixmap)))
            (append-at editor :center
                       (format nil "<p><img src=\"~A\"/></p>" url))))))
    (append-at editor :justify
               (lorem-ipsum:paragraph :prologue nil))))

(defun append-at (editor alignment string)
  (q+:append editor string)
  (let* ((cursor (q+:text-cursor editor))
         (block-format (q+:block-format cursor))
         (alignment (ecase alignment
                      (:left (q+:qt.align-left))
                      (:right (q+:qt.align-right))
                      (:center (q+:qt.align-hcenter))
                      (:justify (q+:qt.align-justify)))))
    (setf (q+:alignment block-format) alignment
          (q+:block-format cursor) block-format)))

(defun svg-pixmap (pathname &key (scale 1) (renderer nil rendererp))
  (let ((namestring (uiop:native-namestring pathname)))
    (if rendererp
        (%svg-pixmap namestring scale renderer)
        (with-finalizing ((renderer (q+:make-qsvgrenderer)))
          (%svg-pixmap namestring scale renderer)))))

(defun %svg-pixmap (svg-namestring scale renderer)
  (q+:load renderer svg-namestring)
  (let* ((size (q+:default-size renderer))
         (x (q+:width size))
         (y (q+:height size))
         (pixmap (q+:make-qpixmap (round (* scale x)) (round (* scale y)))))
    (with-finalizing ((color (q+:make-qcolor 0 0 0 0)))
      (q+:fill pixmap color))
    (with-finalizing ((painter (q+:make-qpainter pixmap)))
      (q+:render renderer painter (q+:make-qrectf (q+:rect pixmap))))
    pixmap))
