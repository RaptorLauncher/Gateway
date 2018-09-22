;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; cable/buffer.lisp

(in-package #:gateway.cable)

(defvar *stream-buffers* (tg:make-weak-hash-table :weakness :key))

(defvar *stream-buffers-lock* (bt:make-lock))

(defvar *stream-buffers-lock-held* nil)

(defmacro with-lock-held (() &body body)
  `(cond (*stream-buffers-lock-held* ,@body)
         (t (bt:with-lock-held (*stream-buffers-lock*)
              (let ((*stream-buffers-lock-held* t))
                ,@body)))))

(defun buffer-of (stream)
  (with-lock-held ()
    (check-type stream stream)
    (gethash stream *stream-buffers* "")))

(defun (setf buffer-of) (new-value stream)
  (check-type stream stream)
  (with-lock-held ()
    (if (or (null new-value) (string= new-value ""))
        (remhash stream *stream-buffers*)
        (setf (gethash stream *stream-buffers*) new-value)))
  new-value)

(defun clean-buffers ()
  (with-lock-held ()
    (loop for stream being the hash-keys of *stream-buffers*
          unless (open-stream-p stream)
            do (remhash stream *stream-buffers*)
          finally (return (values)))))
