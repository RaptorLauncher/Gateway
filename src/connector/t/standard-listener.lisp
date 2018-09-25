;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/t/standard-listener.lisp

(in-package #:gateway.connector/test)
(in-readtable protest/parachute)

;;; Utils

(defun make-listener (handler)
  (make-instance 'standard-listener :handler handler :timeout 0.001))

;;; Test suite

(define-test-case standard-listener
    (:documentation "Test suite for STANDARD-LISTENER."
     :tags (:gateway :listener :standard-listener :suite)))

(define-test standard-listener
  :parent connector)

;;; Unit tests

(define-test-case standard-listener-dead-connection
    (:documentation "Check if a dead connection is automatically cleared ~
from a listener's connection list."
     :tags (:gateway :connection :listener :standard-listener :unit))
  :arrange
  1  "Create a listener."
  2  "Create both sides of a connection."
  3  "Add the first side of connection to the listener."
  4  "Assert the listener is alive."
  5  "Assert there are two connections in the listener."
  :act
  6  "Kill the second side of the connection."
  :assert
  7  "Assert the first side of the connection is dead."
  8  "Assert the second side of the connection is dead."
  9  "Assert the listener's connection list contains only one element."
  10 "Assert the listener is alive.")

(define-test standard-listener-dead-connection
  :parent standard-listener
  (finalized-let* ((listener #1?(make-listener (constantly nil))
                             (kill listener)))
    (destructuring-bind (connection-1 connection-2) #2?(make-connection-pair)
      (unwind-protect
           (progn #3?(add-connection listener connection-1)
                  #4?(false (deadp listener))
                  #5?(is = 1 (connection-count listener))
                  #6?(kill connection-2)
                  #7?(true (wait () (= 0 (connection-count listener))))
                  #8?(false (deadp listener)))
        (kill connection-1)
        (kill connection-2)))))

(define-test-case standard-listener-message
    (:documentation "Tests the message-passing functionality of the ~
STANDARD-LISTENER."
     :tags (:gateway :listener :connection :standard-listener :unit))
  :arrange
  1 "Create a listener with a simple list-pushing handler."
  2 "Create both sides of connection 1."
  3 "Create both sides of connection 2."
  4 "Create both sides of connection 3."
  5 "Add first sides of the three connections to the listener's connection ~
list."
  :act
  6 "Send a message through the other side of connection 1, 2 or 3."
  :assert
  7 "Assert the message was pushed onto the list."
  8 "Pop the message from the list and go back to step 6 a few times.")

(define-test standard-listener-message
  ;; TODO test again and check if socket errors still occur
  :parent standard-listener
  (finalized-let* ((lock (bt:make-lock)) (list '())
                   (fn (lambda (conn data) (bt:with-lock-held (lock)
                                             (push (list conn data) list))))
                   (listener #1?(make-listener fn) (kill listener))
                   (c1 #2?(make-connection-pair) (mapc #'kill c1))
                   (c2 #3?(make-connection-pair) (mapc #'kill c2))
                   (c3 #4?(make-connection-pair) (mapc #'kill c3))
                   (c1a (first c1)) (c1b (second c1))
                   (c2a (first c2)) (c2b (second c2))
                   (c3a (first c3)) (c3b (second c3)))
    #5?(progn (add-connection listener c1a)
              (add-connection listener c2a)
              (add-connection listener c3a))
    (loop with test-fn = (lambda (x) (member x (bt:with-lock-held (lock) list)
                                             :test #'equal :key #'second))
          for i below 30
          for data = (make-list 10 :initial-element i)
          do (progn
               #6?(connection-send (whichever c1b c2b c3b) data)
               #7?(true (wait () (funcall test-fn data)))
               #8?(bt:with-lock-held (lock) (pop list))))))
