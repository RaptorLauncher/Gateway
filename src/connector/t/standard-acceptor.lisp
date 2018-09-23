;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/t/standard-acceptor.lisp

(in-package #:gateway.connector/test)
(in-readtable protest/parachute)

;;; Utils

(defun make-acceptor (handler)
  (make-instance 'standard-acceptor :handler handler))

;;; Test suite

(define-test-case standard-acceptor
    (:documentation "Test suite for STANDARD-ACCEPTOR."
     :tags (:gateway :acceptor :standard-acceptor :suite)))

(define-test standard-acceptor
  :parent connector)

;;; Unit tests

(define-test-case standard-acceptor-unit
    (:documentation "Unit tests for STANDARD-ACCEPTOR."
     :tags (:gateway :acceptor :standard-acceptor :unit))
  :arrange
  1 "Create an acceptor."
  :act
  2 "Create three sockets connected to the acceptor."
  :assert
  3 "Assert the acceptor has accepted three connections.")

(define-test standard-acceptor-unit
  :parent standard-acceptor
  (finalized-let*
      ((connections '() (mapc #'kill connections))
       (lock (bt:make-lock))
       (handler (lambda (x) (bt:with-lock-held (lock)
                              (usocket:socket-close x)
                              (push x connections))))
       (acceptor #1?(make-acceptor handler) (kill acceptor))
       (server-socket (gateway.connector::socket-of acceptor))
       (host (usocket:get-local-address server-socket))
       (port (usocket:get-local-port server-socket))
       (sockets #2?(loop repeat 3 collect (usocket:socket-connect host port))
                (mapc #'usocket:socket-close sockets)))
    #3?(true (wait () (= 3 (length (bt:with-lock-held (lock)
                                     connections)))))))

;;; Protocol tests

(define-test-case standard-acceptor-death
    (:documentation "Test of KILLABLE protocol for STANDARD-ACCEPTOR."
     :tags (:gateway :protocol :killable :acceptor))
  :arrange
  1 "Create an acceptor."
  2 "Assert acceptor is alive."
  :act
  3 "Kill acceptor."
  :assert
  4 "Assert acceptor is dead.")

(define-test standard-acceptor-death
  :parent standard-acceptor
  (let ((acceptor #1?(make-acceptor (constantly nil))))
    (unwind-protect #2?(false (deadp acceptor))
      #3?(kill acceptor)
      #4?(true (wait () (deadp acceptor))))))
