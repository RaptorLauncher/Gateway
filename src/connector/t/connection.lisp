;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/t/package.lisp

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

;;; Unit tests

(define-test-case standard-connection-complete-input
    (:documentation "Unit tests for STANDARD-CONNECTION - complete input."
     :tags (:gateway :connection :standard-connection :unit))
  1 "Create a connection pair."
  2 "Send a message to the first connection."
  3 "Assert the second connection is ready."
  4 "Receive the message."
  5 "Assert the connection is not ready."
  6 "Assert the message is as expected."
  7 "Assert the connection is alive.")

(define-test-case standard-connection-incomplete-input
    (:documentation "Unit tests for STANDARD-CONNECTION - incomplete input."
     :tags (:gateway :connection :standard-connection :unit))
  1 "Create a connection pair."
  2 "Send an incomplete message to the first connection."
  3 "Assert the second connection is ready."
  4 "Receive the message."
  5 "Assert the connection is not ready."
  6 "Assert the message is empty."
  7 "Assert the connection is alive.")

(define-test-case standard-connection-dead
    (:documentation "Unit tests for STANDARD-CONNECTION - dead connection."
     :tags (:gateway :connection :standard-connection :unit))
  1 "Create a connection pair."
  2 "Kill one of the connections."
  3 "Assert the other connection is ready."
  4 "Receive the message."
  5 "Assert the message is empty."
  6 "Assert the connection is dead.")

(define-test standard-connection-complete-input
  (finalized-let* ((conns #1?(make-connection-pair) (mapc #'kill conns)))
    #2?(connection-send (first conns) '(1 2 3 4))
    #3?(true (readyp (second conns)))
    (multiple-value-bind (message alivep) #4?(connection-receive (second conns))
      #5?(false (readyp (second conns)))
      #6?(is cable-equal message '(1 2 3 4))
      #7?(is eq alivep t))))

(define-test standard-connection-incomplete-input
  (finalized-let* ((conns #1?(make-connection-pair) (mapc #'kill conns)))
    #2?(fformat (usocket:socket-stream (first conns)) "(")
    #3?(true (readyp (second conns)))
    (multiple-value-bind (message alivep) #4?(connection-receive (second conns))
      #5?(false (readyp (second conns)))
      #6?(false message)
      #7?(true alivep))))

(define-test standard-connection-dead
  (finalized-let* ((conns #1?(make-connection-pair) (mapc #'kill conns)))
    #2?(kill (first conns))
    #3?(wait () (readyp (second conns)))
    (multiple-value-bind (message alivep) #4?(connection-receive (second conns))
      #5?(false message)
      #6?(false alivep))))
