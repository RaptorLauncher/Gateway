(in-package #:gateway.ui)
(in-readtable :qtools)

(define-widget number-qtextedit (qtextedit qui:fixed-qtextedit) ())

(define-override (number-qtextedit context-menu-event
                                   fixed-qtextedit-context-menu)
                 (event)
  ;; Print the number of the clicked block
  (let* ((position (q+:pos event))
         (cursor (q+:cursor-for-position number-qtextedit position))
         (block-number (q+:block-number cursor)))
    (print block-number))
  (call-next-method))

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
         (block-format (q+:block-format cursor)))
    (setf (q+:alignment block-format) alignment
          (q+:block-format cursor) block-format)))
