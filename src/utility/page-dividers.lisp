(ql:quickload '(lorem-ipsum qtools qtcore qtgui qtsvg))
(named-readtables:in-readtable :qtools)
(in-package #:cl+qt)

;;; FIXED-QTEXTEDIT

(define-widget fixed-qtextedit (qtextedit) ())

(define-subwidget (fixed-qtextedit fix-context-menu-widget) (q+:make-qwidget))

(define-override (fixed-qtextedit context-menu-event) (event)
  (let ((position (q+:pos event)))
    ;; I have no idea why it works this way, but it seems to work.!
    (call-next-qmethod)
    ;; Display the context menu.
    (with-finalizing ((menu (q+:create-standard-context-menu fixed-qtextedit)))
      (q+:exec menu (q+:global-pos event)))
    ;; Work around the bug.
    (q+:show fix-context-menu-widget)
    (setf (q+:focus fix-context-menu-widget) 0)
    (q+:hide fix-context-menu-widget)
    ;; Fix cursor position after the context menu disappears.
    (let* ((cursor (q+:cursor-for-position fixed-qtextedit position)))
      (setf (q+:text-cursor fixed-qtextedit) cursor))))

;;; Test

(defparameter *page-divider-pathname*
  #p"/home/phoe/Projects/Lisp/Gateway-UI-Assets/Page-Dividers/*.svg")

(defparameter *page-divider-namestring*
  (uiop:native-namestring
   (merge-pathnames "webdesignhot-1-1.svg" *page-divider-pathname*)))

(defun test ()
  (with-main-window (editor 'fixed-qtextedit)
    (let ((document (q+:document editor))
          (pathnames (directory *page-divider-pathname*))
          (prologue t))
      (with-finalizing ((renderer (q+:make-qsvgrenderer)))
        (dolist (pathname pathnames)
          (let ((namestring (uiop:native-namestring pathname))
                (url (uiop:strcat "gateway://" (pathname-name pathname))))
            (append-at editor :justify
                       (lorem-ipsum:paragraph :prologue prologue
                                              :word-count 60))
            (setf prologue nil)
            (with-finalizing ((pixmap (svg-pixmap namestring
                                                  :renderer renderer)))
              (q+:add-resource document (q+:qtextdocument.image-resource)
                               (q+:make-qurl url) (q+:to-image pixmap))
              (append-at editor :center
                         (format nil "<p><img src=\"~A\"/></p>" url))))))
      (append-at editor :justify
                 (lorem-ipsum:paragraph :prologue prologue
                                        :word-count 60)))))

(defun append-at (editor alignment object)
  (q+:append editor object)
  (let* ((cursor (q+:text-cursor editor))
         (block-format (q+:block-format cursor))
         (alignment (ecase alignment
                      (:left (q+:qt.align-left))
                      (:right (q+:qt.align-right))
                      (:center (q+:qt.align-hcenter))
                      (:justify (q+:qt.align-justify)))))
    (setf (q+:alignment block-format) alignment
          (q+:block-format cursor) block-format)))

(defun svg-pixmap (svg-namestring &key (scale 1) (renderer nil rendererp))
  ;; TODO optimize: make it possible to pass a renderer to this function and do
  ;; not finalize it in that case, instead of making a new renderer on each call
  ;; TODO this is already optimized; use it up there
  (if rendererp
      (%svg-pixmap svg-namestring scale renderer)
      (with-finalizing ((renderer (q+:make-qsvgrenderer)))
        (%svg-pixmap svg-namestring scale renderer))))

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
