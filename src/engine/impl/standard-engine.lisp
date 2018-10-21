;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; engine/impl/standard-engine.lisp

(in-package :gateway/impl)

(defclass standard-engine (engine)
  ((%engine :accessor %engine)
   (%channel :accessor channel)
   (%cleaner :accessor cleaner)
   (%handler :accessor handler
             :initarg :handler
             :initform (error "Must provide a handler function."))
   (%name :accessor name))
  (:documentation #.(format nil "A standard implementation of Gateway protocol ~
class ENGINE.

This engine contains a lparallel engine. The engine workers each call the ~
handler function with the message as its argument. That function executes a ~
single iteration of the engine's job and is not meant to loop.

TODO these below lines belong to the framework, not the class

The default implementation of the handler function is the exported function ~
#'STANDARD-ENGINE-OPERATION.

The messages are expected to be in form (CONNECTION COMMAND . REST), where ~
CONNECTION is a connection object from which MESSAGE came; REST is ignored ~
and reserved for future use.")))

(define-print (standard-engine stream)
  (if (alivep standard-engine)
      (format stream "(~D workers, ALIVE)"
              (worker-count standard-engine))
      (format stream "(DEAD)")))

(defun worker-count (engine)
  (check-type engine standard-engine)
  (let ((lparallel:*engine* (%engine engine)))
    (lparallel:engine-worker-count)))

(define-constructor (standard-engine threads)
  (let* ((threads (or threads
                      (config :engine-threads)
                      (cl-cpus:get-number-of-processors)))
         (name "Gateway - Engine, ~D threads" cpus))
    (v:trace :gateway "Standard engine starting with ~D threads." threads)
    (setf (name standard-engine) name
          (%engine standard-engine)
          (lparallel:make-engine threads
                                 :name (cat name " (lparallel engine)")))
    (let ((lparallel:*engine* (%engine standard-engine)))
      (setf (channel standard-engine) (lparallel:make-channel)
            (cleaner standard-engine)
            (make-thread
             (lambda ()
               (loop (lparallel:receive-result (channel standard-engine))))
             :name "Gateway - Engine cleaner thread")))))

(defmethod deadp ((engine standard-engine))
  (not (lparallel.engine::alivep (%engine engine))))

(defmethod kill ((engine standard-engine))
  (v:trace :gateway "Standard engine was killed.")
  (let ((lparallel:*engine* (%engine engine)))
    (lparallel:end-engine :wait t))
  (destroy-thread (cleaner engine))
  (values))

(defmethod enqueue ((engine standard-engine) message)
  (lparallel:submit-task (channel engine) (handler engine) message)
  t)

;;; TESTS

;; (in-readtable protest)

;; TODO separate tests from code

;; (define-test-case standard-engine-death
;;     (:description "Test of KILLABLE protocol for STANDARD-LISTENER."
;;      :tags (:protocol :killable :engine)
;;      :type :protocol)
;;   :arrange
;;   1 "Create a engine."
;;   2 "Assert the engine is alive."
;;   :act
;;   3 "Kill the engine."
;;   :assert
;;   4 "Assert the engine is dead.")

;; (define-test standard-engine-death
;;   (let* ((engine #1?(make-instance 'standard-engine :handler (constantly nil))))
;;     #2?(is (alivep engine))
;;     #3?(kill engine)
;;     #4?(is (wait () (deadp engine)))))

;; (define-test-case standard-engine
;;     (:description "Test the engine's enqueueing functionality."
;;      :tags (:implementation :engine)
;;      :type :implementation)
;;   :arrange
;;   1 "Create a handler that atomically (with a lock) increases a variable that ~
;; is initially 0."
;;   2 "Create a engine."
;;   3 "Prepare a shuffled list of integers from 1 to 100."
;;   :act
;;   4 "Submit 100 tasks to the engine which increase the variable by an integer."
;;   :assert
;;   5 "Assert the variable is = to 5050."
;;   6 "Assert no more results are available in the channel.")

;; (define-test standard-engine
;;   (finalized-let*
;;       ((var 0)
;;        (lock (make-lock "STANDARD-ENGINE test lock"))
;;        (handler #1?(lambda (x) (with-lock-held (lock) (incf var x))))
;;        (engine #2?(make-instance 'standard-engine :handler handler)
;;                (kill engine))
;;        (tasks #3?(shuffle (iota 100 :start 1))))
;;     #4?(dolist (i tasks)
;;          (is (enqueue engine i)))
;;     #5?(is (wait () (with-lock-held (lock) (= var 5050))))
;;     (sleep 0.1)
;;     #6?(is (wait () (equal '(nil nil)
;;                            (multiple-value-list
;;                             (lparallel.queue:peek-queue
;;                              (lparallel.engine::channel-queue
;;                               (channel engine)))))))))
