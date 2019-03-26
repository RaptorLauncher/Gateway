;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; engine/t/standard-engine.lisp

(in-package #:gateway.engine/test)
(in-readtable protest/parachute)

;; TODO add a parent for all Gateway test cases
(define-test-case standard-engine
    (:documentation "Test suite for STANDARD-ENGINE."
     :tags (:gateway :engine :standard-engine :suite)))

(define-test standard-engine
  :parent engine)

;;; Protocol tests

(define-test-case standard-engine-death
    (:documentation "Test of KILLABLE protocol for STANDARD-LISTENER."
     :tags (:gateway :protocol :killable :engine))
  :arrange
  1 "Create a engine."
  2 "Assert the engine is alive."
  :act
  3 "Kill the engine."
  :assert
  4 "Assert the engine is dead.")

(define-test standard-engine-death
  (let* ((engine #1?(make-instance 'standard-engine :handler (constantly nil))))
    #2?(false (deadp engine))
    #3?(kill engine)
    #4?(true (wait () (deadp engine)))))

;;; Unit tests

(define-test-case standard-engine
    (:documentation "Test the engine's message-accepting functionality."
     :tags (:gateway :engine :standard-engine :unit))
  :arrange
  1 "Create a handler that atomically (with a lock) increases a variable that ~
is initially 0."
  2 "Create a engine."
  3 "Prepare a shuffled list of integers from 1 to 100."
  :act
  4 "Submit 100 tasks to the engine which increase the variable by an integer."
  :assert
  5 "Assert the variable was set to 5050."
  6 "Assert no more results are available in the channel.")

(defun engine-kernel-queue-empty-p (engine)
  (not (nth-value 1 (lparallel.queue:peek-queue
                     (lparallel.kernel::channel-queue
                      (gateway.engine/impl::channel engine))))))

(define-test standard-engine
  (finalized-let*
      ((var 0)
       (lock (bt:make-lock "STANDARD-ENGINE test lock"))
       (handler #1?(lambda (x y) (declare (ignore y))
                     (bt:with-lock-held (lock) (incf var x))))
       (engine #2?(make-instance 'standard-engine :handler handler)
               (kill engine))
       (tasks #3?(shuffle (iota 100 :start 1))))
    #4?(dolist (i tasks) (accept-message engine i nil))
    #5?(true (wait () (bt:with-lock-held (lock) (= var 5050))))
    #6?(true (wait () (engine-kernel-queue-empty-p engine)))))
