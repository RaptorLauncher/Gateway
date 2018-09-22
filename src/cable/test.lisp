;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; cable/test.lisp

(defpackage #:gateway.cable/test
  (:use #:cl #:gateway.cable #:1am)
  (:shadow #:run)
  (:export #:run))

(in-package #:gateway.cable/test)

(defun test-buffer ()
  (let ((gateway.cable::*stream-buffers*
          (tg:make-weak-hash-table :weakness :key)))
    (loop repeat 10
          for stream = (make-string-input-stream (make-string 10))
          do (setf (buffer-of stream) "12345")
             (close stream))
    (clean-buffers)
    (assert (= 0 (hash-table-count gateway.cable::*stream-buffers*)))))

(defun test-output ()
  (let* ((input '(1 2.0 "foo BAR \\ \" baz" foo :foo #:foo (1 2 3) (4 5)))
         (output "(1 2.0 \"foo BAR \\\\ \\\" baz\" FOO FOO FOO (1 2 3) (4 5))")
         (result (with-output-to-string (s) (to-cable input s))))
    (assert (string= result output))))

(defun run ()
  (test-buffer)
  (test-output)
  t)
