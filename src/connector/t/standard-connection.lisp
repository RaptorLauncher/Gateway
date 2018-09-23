;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/t/standard-connection.lisp

(in-package #:gateway.connector/test)
(in-readtable protest/parachute)

;;; Utils

(defun make-connection-pair ()
  (let* ((socket-listen (usocket:socket-listen "127.0.0.1" 0))
         (port (usocket:get-local-port socket-listen))
         (socket-connect (usocket:socket-connect "127.0.0.1" port))
         (socket-accept (usocket:socket-accept socket-listen)))
    (usocket:socket-close socket-listen)
    (list (change-class socket-connect 'standard-connection)
          (change-class socket-accept'standard-connection))))

;;; Test suite

(define-test-case standard-connection
    (:documentation "Test suite for STANDARD-CONNECTION."
     :tags (:gateway :connection :standard-connection :suite)))

(define-test standard-connection
  :parent connector)

;;; Unit tests

(define-test-case standard-connection-complete-input
    (:documentation "Unit tests for STANDARD-CONNECTION - complete input."
     :tags (:gateway :connection :standard-connection :unit))
  :arrange
  1 "Create a connection pair."
  2 "Send a message to the first connection."
  3 "Assert the second connection is ready."
  :act
  4 "Receive the message."
  :assert
  5 "Assert the connection is not ready."
  6 "Assert the message is as expected."
  7 "Assert the connection is alive.")

(define-test-case standard-connection-incomplete-input
    (:documentation "Unit tests for STANDARD-CONNECTION - incomplete input."
     :tags (:gateway :connection :standard-connection :unit))
  :arrange
  1 "Create a connection pair."
  2 "Send an incomplete message to the first connection."
  3 "Assert the second connection is ready."
  :act
  4 "Receive the message."
  :assert
  5 "Assert the connection is not ready."
  6 "Assert the message is empty."
  7 "Assert the connection is alive.")

(define-test-case standard-connection-dead
    (:documentation "Unit tests for STANDARD-CONNECTION - dead connection."
     :tags (:gateway :connection :standard-connection :unit))
  :arrange
  1 "Create a connection pair."
  2 "Kill one of the connections."
  3 "Assert the other connection is ready."
  :act
  4 "Receive the message."
  :assert
  5 "Assert the message is empty."
  6 "Assert the connection is dead.")

(define-test standard-connection-complete-input
  :parent standard-connection
  (finalized-let* ((conns #1?(make-connection-pair) (mapc #'kill conns)))
    #2?(connection-send (first conns) '(1 2 3 4))
    #3?(true (readyp (second conns)))
    (multiple-value-bind (message alivep) #4?(connection-receive (second conns))
      #5?(false (readyp (second conns)))
      #6?(is cable-equal message '(1 2 3 4))
      #7?(is eq alivep t))))

(define-test standard-connection-incomplete-input
  :parent standard-connection
  (finalized-let* ((conns #1?(make-connection-pair) (mapc #'kill conns)))
    #2?(fformat (usocket:socket-stream (first conns)) "(")
    #3?(true (readyp (second conns)))
    (multiple-value-bind (message alivep) #4?(connection-receive (second conns))
      #5?(false (readyp (second conns)))
      #6?(false message)
      #7?(true alivep))))

(define-test standard-connection-dead
  :parent standard-connection
  (finalized-let* ((conns #1?(make-connection-pair) (mapc #'kill conns)))
    #2?(kill (first conns))
    #3?(true (wait () (readyp (second conns))))
    (multiple-value-bind (message alivep) #4?(connection-receive (second conns))
      #5?(false message)
      #6?(false alivep))))

;;; Protocol tests

(define-test-case standard-connection-death
    (:documentation "Test of KILLABLE protocol for STANDARD-CONNECTIONs."
     :tags (:gateway :protocol :killable :connection))
  :arrange
  1 "Create connections."
  2 "Assert connection 1 is alive."
  3 "Assert connection 2 is alive."
  :act
  4  "Kill connection 1."
  :assert
  5 "Assert connection 1 is dead."
  6 "Assert connection 2 is dead.")

(define-test standard-connection-death
  :parent standard-connection
  (finalized-let* ((conns #1?(make-connection-pair) (mapc #'kill conns)))
    #2?(false (deadp (first conns)))
    #3?(false (deadp (second conns)))
    #4?(kill (first conns))
    #5?(true (deadp (first conns)))
    #6?(true (deadp (second conns)))))

(define-test-case standard-connection-send-receive
    (:documentation "Test of sending/receiving data by STANDARD-CONNECTIONs."
     :tags (:gateway :connection :protocol :standard-connection))
  :arrange
  1 "Create a connection pair."
  :act
  2 "Send test data from connection 1."
  :assert
  3 "Assert connection 2 is ready."
  4 "Assert the received data matches the data that was sent.")

(defparameter *standard-connection-send-receive-data*
  '((1 2 3 4 5 6 7 8 9 0)
    (#:a #:b #:c #:d #:e #:f (#:g) ((((#:h #:i #:j #:k (#:l 2000) #:m 3.0d0)))))
    ("Jackdaws loves my big sphinx of quarts.")
    (#:lorem #:ipsum #:dolor #:sit #:amet)
    ("a" #:a "a" #:a "a" "b")))

(define-test standard-connection-send-receive
  :parent standard-connection
  (finalized-let* ((conns #1?(make-connection-pair) (mapc #'kill conns)))
    (labels ((test-case (x y data)
               #2?(connection-send x data)
               #3?(true (wait () (readyp y)))
               #4?(is cable-equal data (connection-receive y)))
             (test (x y) (mapc (alexandria:curry #'test-case x y)
                               *standard-connection-send-receive-data*)))
      (test (first conns) (second conns))
      (test (second conns) (first conns)))))
