;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/t/standard-writer.lisp

(in-package #:gateway.connector/test)
(in-readtable protest/parachute)

;;; Utils

(defun make-writer ()
  (make-instance 'standard-writer))

;;; Test suite

(define-test-case standard-writer
    (:documentation "Test suite for STANDARD-WRITER."
     :tags (:gateway :writer :standard-writer :suite)))

(define-test standard-writer
  :parent connector)

;;; Unit tests

(define-test-case standard-writer-write-data
    (:documentation "Tests the data-writing functionality of the ~
STANDARD-WRITER."
     :tags (:gateway :writer :connection :standard-writer :unit))
  :arrange
  1 "Create a writer."
  2 "Create both sides of connection 1."
  3 "Create both sides of connection 2."
  4 "Create both sides of connection 3."
  :act
  5 "Write semi-random data to one side of the the connection."
  :assert
  6 "Assert the data was received from the other end of the connection.")

(define-test standard-writer-write-data
  :parent standard-writer
  (finalized-let* ((writer #1?(make-writer) (kill writer))
                   (c1 #2?(make-connection-pair) (mapc #'kill c1))
                   (c2 #3?(make-connection-pair) (mapc #'kill c2))
                   (c3 #4?(make-connection-pair) (mapc #'kill c3)))
    (dotimes (i 30)
      (let ((data (list (random 10000) '#:foo "bar" (random 10000)))
            (conns (whichever c1 c2 c3)))
        #5?(write-data writer (first conns) data)
        #6?(is cable-equal data
               (wait () (connection-receive (second conns))))))))

;;; Protocol tests

(define-test-case standard-writer-death
    (:documentation "Test of KILLABLE protocol for STANDARD-WRITER."
     :tags (:gateway :protocol :killable :writer))
  :arrange
  1 "Create a writer."
  2 "Assert the writer is alive."
  :act
  3 "Kill the writer."
  :assert
  4 "Assert the writer is dead.")

(define-test standard-writer-death
  :parent standard-writer
  (let ((writer #1?(make-writer)))
    (unwind-protect #2?(false (deadp writer))
      #3?(kill writer)
      #4?(true (wait () (deadp writer))))))
