;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; cable/test.lisp

(defpackage #:gateway.cable/test
  (:use #:cl #:gateway.cable)
  (:shadow #:run)
  (:export #:run))

(in-package #:gateway.cable/test)

(defun run ()
  (test-equal)
  (test-buffer)
  (test-output)
  (test-input)
  (test-buffered-input)
  t)

(defvar *input-sexp*
  '(1 2.0d0 "foo BAR \\ \" baz" foo :foo #:foo (1 2 3) (4 5)))

(defvar *input-string*
  "(1 2.0 \"foo BAR \\\\ \\\" baz\" FOO FOO FOO (1 2 3) (4 5))")

(defun test-equal ()
  (assert (cable-equal '#:foo :foo))
  (let ((data '(1 (2 3) (4.0 5) "foo" "FOO" #:foo :foo foo)))
    (assert (cable-equal data data))))

(defun test-buffer ()
  (let ((gateway.cable::*stream-buffers*
          (tg:make-weak-hash-table :weakness :key)))
    (loop repeat 10
          for stream = (make-string-input-stream (make-string 10))
          do (setf (gateway.cable::buffer-of stream) "12345")
             (close stream))
    (clean-buffers)
    (assert (= 0 (hash-table-count gateway.cable::*stream-buffers*)))))

(defun test-output ()
  (let* ((result (with-output-to-string (s) (to-cable *input-sexp* s))))
    (assert (string= result *input-string*))))

(defun test-input ()
  (let* ((result (with-input-from-string (s *input-string*) (from-cable s))))
    (assert (cable-equal result *input-sexp*))))

(defun test-buffered-input ()
  (let ((pipe (cl-plumbing:make-pipe)))
    (loop for char across "(1 2 3 "
          do (write-char char pipe))
    (assert (null (from-cable-buffered pipe)))
    (loop for char across "4 5 6)"
          do (write-char char pipe))
    (assert (equal '(1 2 3 4 5 6) (from-cable-buffered pipe)))))
