;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/t/standard-connector.lisp

(in-package #:gateway.connector/test)
(in-readtable protest/parachute)

;;; Utils

(defun make-connector (handler)
  (make-instance 'standard-connector :listener-handler handler))

(defun make-connection (host port)
  (change-class (usocket:socket-connect host port) 'standard-connection))

;;; Test suite

(define-test-case standard-connector
    (:documentation "Test suite for STANDARD-CONNECTOR."
     :tags (:gateway :connector :standard-connector :suite)))

(define-test standard-connector
  :parent connector)

;;; Integration tests

(define-test-case standard-connector-echo
    (:documentation "Test of basic echo functionality for STANDARD-CONNECTOR."
     :tags (:gateway :integration :standard-connector))
  :arrange
  1 "Create a connector with an echo listener-handler."
  2 "Create ten connections to the connector."
  :act
  3 "Send semi-random data on a random connection."
  :assert
  4 "Assert that the data was echoed back on the connection.")

(define-test standard-connector-echo
  :parent standard-connector
  (let* ((writer (make-instance 'standard-writer)))
    (flet ((fn (connection data)
             (write-data writer connection data)))
      (finalized-let*
          ((connector #1?(make-connector #'fn) (kill connector))
           (socket (gateway.connector::socket-of (first (acceptors connector))))
           (host (usocket:get-local-address socket))
           (port (usocket:get-local-port socket))
           (conns #2?(loop repeat 10 collect (make-connection host port))
                  (mapc #'kill conns)))
        (loop repeat 30
              for data = (list (random 10000) '#:foo "bar" (random 10000))
              for conn = (elt conns (random (length conns)))
              do #3?(connection-send conn data)
              #4?(true (wait ()
                         (cable-equal data (connection-receive conn)))))))))

;;; Protocol tests

(define-test-case standard-connector-death
    (:documentation "Test of KILLABLE protocol for STANDARD-CONNECTOR."
     :tags (:gateway :protocol :killable :connector))
  :arrange
  1 "Create a connector."
  2 "Assert the connector is alive."
  3 "Assert the connector's acceptor is alive."
  4 "Assert the connector's listener is alive."
  5 "Assert the connector's writer is alive."
  :act
  6  "Kill the connector."
  :assert
  7 "Assert the connector is dead."
  8 "Assert the connector's acceptor is dead."
  9 "Assert the connector's listener is dead."
  10 "Assert the connector's writer is dead.")

(define-test standard-connector-death
  :parent standard-connector
  (let ((connector #1?(make-connector (constantly nil))))
    (unwind-protect (progn #2?(true (wait () (not (deadp connector))))
                           #3?(false (every #'deadp (acceptors connector)))
                           #4?(false (every #'deadp (listeners connector)))
                           #5?(false (every #'deadp (writers connector))))
      #6?(kill connector)
      #7?(true (wait () (deadp connector)))
      #8?(true (every #'deadp (acceptors connector)))
      #9?(true (every #'deadp (listeners connector)))
      #10?(true (every #'deadp (writers connector))))))
