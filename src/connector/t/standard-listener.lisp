;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/t/standard-listener.lisp

(in-package #:gateway.connector/test)
(in-readtable protest/parachute)

;;; Utils

(defun make-listener (handler)
  (make-instance 'standard-listener :handler handler))

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
      #3?(add-connection listener connection-1)
      #4?(false (deadp listener))
      #5?(is = 2 (connection-count listener))
      #6?(kill connection-2)
      #7?(true (wait () (deadp connection-1)))
      #8?(true (deadp connection-2))
      #9?(true (wait () (= 1 (connection-count listener))))
      #10?(false (deadp listener)))))

(defun my-test ()
  (finalized-let* ((listener (make-listener (constantly nil))
                             (kill listener)))
    (destructuring-bind (connection-1 connection-2) (make-connection-pair)
      (add-connection listener connection-1)
      (assert (not (deadp listener)))
      (assert (= 2 (connection-count listener)))
      (kill connection-2)
      (assert (wait () (deadp connection-1)))
      (assert (deadp connection-2))
      (assert (wait (4) (= 1 (connection-count listener))))
      (assert (not (deadp listener))))))

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
  :loop-act
  6 "Send a message through the other side of connection 1, 2 or 3."
  :loop-assert
  7 "Assert the message was pushed onto the list."
  8 "Pop the message from the list and go back to step 6 a few times.")

(define-test standard-listener-message
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
    (loop for i below 30
          for data = (make-list 10 :initial-element i)
          do (progn
               #6?(connection-send (whichever c1b c2b c3b) data)
               #7?(true (wait () (member data (bt:with-lock-held (lock) list)
                                         :test #'equal :key #'second)))
               #8?(bt:with-lock-held (lock) (pop list))))))

;;; Protocol tests

(define-test-case standard-listener-death
    (:documentation "Test of KILLABLE protocol for STANDARD-LISTENER."
     :tags (:gateway :protocol :killable :listener))
  :arrange
  1  "Create a listener."
  2  "Get the reference to one side of the listener's notifier connection."
  3  "Get the reference to other side of the listener's notifier connection."
  4  "Create a connection."
  5  "Add the connection to the listener."
  6  "Assert the listener is alive."
  7  "Assert one side of the listener's notifier connection is alive."
  8  "Assert the other side of the listener's notifier connection is alive."
  :act
  9  "Kill the listener."
  :assert
  10 "Assert the listener is dead."
  11 "Assert the one side of the listener's notifier connection is dead."
  12 "Assert the other side of the listener's notifier connection is dead."
  13 "Assert the connection is dead.")

(define-test standard-listener-death
  :parent standard-listener
  (let* ((listener #1?(make-listener (constantly nil)))
         (notifier-1 #2?(gateway.connector::notifier-connection listener))
         (notifier-2 #3?(first (gateway.connector::connections listener))))
    (destructuring-bind (connection-1 connection-2) #4?(make-connection-pair)
      (declare (ignore connection-2))
      #5?(add-connection listener connection-1)
      #6?(false (deadp listener))
      #7?(false (deadp notifier-1))
      #8?(false (deadp notifier-2))
      #9?(kill listener)
      #10?(true (wait () (deadp listener)))
      #11?(true (deadp notifier-1))
      #12?(true (deadp notifier-2))
      #13?(true (deadp connection-1)))))
