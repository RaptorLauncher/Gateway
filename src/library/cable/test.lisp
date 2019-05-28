;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; cable/test.lisp

(uiop:define-package #:gateway.cable/test
  (:use #:cl
        #:gateway.cable
        #:protest/parachute)
  (:import-from #:protest/test-case
                #:define-test-case))

(in-package #:gateway.cable/test)

(define-test-case cable
    (:documentation "Test suite for Gateway cable protocol."
     :tags (:gateway :cable :suite)))

(define-test cable
  :parent (#:gateway.init #:gateway-full-test))

(defvar *input-sexp*
  '(1 2.0d0 "foo BAR \\ \" baz" foo :foo #:foo (1 2 3) (4 5)))

(defvar *input-string*
  "(1 2.0 \"foo BAR \\\\ \\\" baz\" FOO FOO FOO (1 2 3) (4 5))")

(define-test-case cable-equal
    (:documentation "Check whether the CABLE-EQUAL predicate works."
     :tags (:gateway :cable :unit-test)))

(define-test cable-equal
  :parent cable
  (is cable-equal '#:foo :foo)
  (let ((data '(1 (2 3) (4.0 5) "foo" "FOO" #:foo :foo foo)))
    (is cable-equal data data)))

(define-test-case cable-buffer
    (:documentation "Check whether the stream buffering works."
     :tags (:gateway :cable :unit-test)))

(define-test cable-buffer
  :parent cable
  (let ((gateway.cable::*stream-buffers*
          (tg:make-weak-hash-table :weakness :key)))
    (loop repeat 10
          for stream = (make-string-input-stream (make-string 10))
          do (setf (gateway.cable::buffer-of stream) "12345")
             (close stream))
    (clean-buffers)
    (is = 0 (hash-table-count gateway.cable::*stream-buffers*))))

(define-test-case to-cable
    (:documentation "Check whether TO-CABLE output works."
     :tags (:gateway :cable :unit-test)))

(define-test to-cable
  :parent cable
  (let* ((result (with-output-to-string (s) (to-cable *input-sexp* s))))
    (is string= result *input-string*)))

(define-test-case from-cable
    (:documentation "Check whether FROM-CABLE input works."
     :tags (:gateway :cable :unit-test)))

(define-test from-cable
  :parent cable
  (let* ((result (with-input-from-string (s *input-string*) (from-cable s))))
    (is cable-equal result *input-sexp*)))

(define-test-case cable-buffered-input
    (:documentation "Check whether reading from buffered input works."
     :tags (:gateway :cable :unit-test)))

(define-test cable-buffered-input
  :parent cable
  (flet ((test-buffered-input (left right expected)
           (let ((pipe (cl-plumbing:make-pipe)))
             (loop for char across left
                   do (write-char char pipe))
             (true (null (from-cable-buffered pipe)))
             (loop for char across right
                   do (write-char char pipe))
             (is equal expected (from-cable-buffered pipe)))))
    (test-buffered-input "(1 2 3 " "4 5 6)" '(1 2 3 4 5 6))
    (test-buffered-input "(1 2 3" " 4 5 6)" '(1 2 3 4 5 6))))

(define-test-case cable-read-limit
    (:documentation "Check whether read limits work."
     :tags (:gateway :cable :unit-test)))

(define-test cable-read-limit
  :parent cable
  (let ((success-inputs '("1234567890"
                          "(asdf 123)"
                          "         1"))
        (failure-inputs '("12345678901"
                          "(asdf 1234)"
                          "          1")))
    (flet ((test-success (string)
             (let ((*read-limit* 10))
               (true (from-cable (make-string-input-stream string)))))
           (test-failure (string)
             (let ((*read-limit* 10))
               (fail (from-cable (make-string-input-stream string))
                   read-limit-hit))))
      (mapc #'test-success success-inputs)
      (mapc #'test-failure failure-inputs))))

(define-test-case cable-depth-limit
    (:documentation "Check whether depth limits work."
     :tags (:gateway :cable :unit-test)))

(define-test cable-depth-limit
  :parent cable
  (let ((*depth-limit* 4)
        (depth-4-stream (make-string-input-stream "(((())))"))
        (depth-5-stream (make-string-input-stream "((((()))))")))
    (true (from-cable depth-4-stream))
    (fail (from-cable depth-5-stream) depth-limit-hit)))
