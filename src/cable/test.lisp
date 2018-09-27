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
  (test-read-limit)
  (test-depth-limit)
  t)

(defmacro is (form)
  `(progn (assert ,form) (princ ".")))

(defvar *input-sexp*
  '(1 2.0d0 "foo BAR \\ \" baz" foo :foo #:foo (1 2 3) (4 5)))

(defvar *input-string*
  "(1 2.0 \"foo BAR \\\\ \\\" baz\" FOO FOO FOO (1 2 3) (4 5))")

(defun test-equal ()
  (is (cable-equal '#:foo :foo))
  (let ((data '(1 (2 3) (4.0 5) "foo" "FOO" #:foo :foo foo)))
    (is (cable-equal data data))))

(defun test-buffer ()
  (let ((gateway.cable::*stream-buffers*
          (tg:make-weak-hash-table :weakness :key)))
    (loop repeat 10
          for stream = (make-string-input-stream (make-string 10))
          do (setf (gateway.cable::buffer-of stream) "12345")
             (close stream))
    (clean-buffers)
    (is (= 0 (hash-table-count gateway.cable::*stream-buffers*)))))

(defun test-output ()
  (let* ((result (with-output-to-string (s) (to-cable *input-sexp* s))))
    (is (string= result *input-string*))))

(defun test-input ()
  (let* ((result (with-input-from-string (s *input-string*) (from-cable s))))
    (is (cable-equal result *input-sexp*))))

(defun test-buffered-input ()
  (%test-buffered-input "(1 2 3 " "4 5 6)" '(1 2 3 4 5 6))
  (%test-buffered-input "(1 2 3" " 4 5 6)" '(1 2 3 4 5 6)))

(defun %test-buffered-input (left right expected)
  (let ((pipe (cl-plumbing:make-pipe)))
    (loop for char across left
          do (write-char char pipe))
    (is (null (from-cable-buffered pipe)))
    (loop for char across right
          do (write-char char pipe))
    (is (equal expected (from-cable-buffered pipe)))))

(defun test-read-limit ()
  (let ((success-inputs '("1234567890"
                          "(asdf 123)"
                          "         1"))
        (failure-inputs '("12345678901"
                          "(asdf 1234)"
                          "          1")))
    (mapc #'%test-read-limit-success success-inputs)
    (mapc #'%test-read-limit-failure failure-inputs)))

(defun %test-read-limit-success (string)
  (let ((*read-limit* 10))
    (from-cable (make-string-input-stream string))))

(defun %test-read-limit-failure (string)
  (let ((*read-limit* 10))
    (handler-case (progn (from-cable (make-string-input-stream string))
                         (error 'error))
      (read-limit-hit ()))))

(defun test-depth-limit ()
  (let ((*depth-limit* 4))
    (from-cable (make-string-input-stream "(((())))"))
    (handler-case (progn (from-cable (make-string-input-stream "((((()))))"))
                         (error 'error))
      (depth-limit-hit ()))))
