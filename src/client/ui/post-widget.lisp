;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017-2020
;;;; post-widget.lisp

(define-widget post-widget (qtextbrowser)
  ((post :accessor post :initarg :post))
  (:default-initargs :post nil))

(define-widget post-widget-view (qwidget) ())

(define-subwidget (post-widget-view layout)
    (q+:make-qvboxlayout post-widget-view))

(defun set-color (main-window post-widget)
  (flet ((color-hex (color)
           (logior (ash (q+:red color) 16)
                   (ash (q+:green color) 8)
                   (q+:blue color))))
    (let ((window (q+:color (q+:palette main-window) (q+:qpalette.window)))
          (text (q+:color (q+:palette main-window) (q+:qpalette.window-text))))
      (setf (q+:style-sheet post-widget)
            (format nil "QTextEdit { background-color: #~6,'0X; color: #~6,'0X }"
                    (color-hex window) (color-hex text))))))

(defun test-post-widget ()
  (with-main-window (main-window 'post-widget-view)
    (with-slots-bound (main-window post-widget-view)
      (dotimes (i 3)
        (let ((post-widget (make-instance 'post-widget)))
          (setf (q+:html post-widget) "<p>asdflheiwfognisgo</p>
<p>asdflheiwfognisgo</p>
<p>asdflheiwfognisgo</p>
<p>asdflheiwfognisgo</p>")
          (q+:add-widget layout post-widget)
          (set-color main-window post-widget)))
      (q+:add-widget layout (q+:make-qlabel "asdsfsfasf")))))
