;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; cable/test.lisp

(defpackage #:gateway.cable/test
  (:use #:cl #:gateway.cable #:1am)
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

(defun test-equal ()
  (assert (cable-equal '(1 (2 3) (4.0 5) "foo" "FOO" #:foo :foo foo)
                       '(1 (2 3) (4.0 5) "foo" "FOO" #:foo :foo foo))))

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
  (let* ((input '(1 2.0d0 "foo BAR \\ \" baz" foo :foo #:foo (1 2 3) (4 5)))
         (output "(1 2.0 \"foo BAR \\\\ \\\" baz\" FOO FOO FOO (1 2 3) (4 5))")
         (result (with-output-to-string (s) (to-cable input s))))
    (assert (string= result output))))

(defun test-input ()
  (let* ((input "(1 2.0 \"foo BAR \\\\ \\\" baz\" FOO (1 2 3) (4 5))")
         (output '(1 2.0d0 "foo BAR \\ \" baz" #:foo (1 2 3) (4 5)))
         (result (with-input-from-string (s input) (from-cable s))))
    (assert (cable-equal result output))))

(defun test-buffered-input ()
  (let ((pipe (cl-plumbing:make-pipe)))
    (loop for char across "(1 2 3 "
          do (write-char char pipe))
    (assert (null (from-cable-buffered pipe)))
    (loop for char across "4 5 6)"
          do (write-char char pipe))
    (assert (equal '(1 2 3 4 5 6) (from-cable-buffered pipe)))))
