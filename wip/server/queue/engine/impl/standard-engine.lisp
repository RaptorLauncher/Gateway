;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; engine/impl/standard-engine.lisp

(in-package :gateway.engine/impl)

(defclass standard-engine (engine)
  ((%kernel :accessor %kernel)
   (%channel :accessor channel)
   (%cleaner :accessor cleaner)
   (%handler :initarg :handler)
   (%name :accessor name))
  (:documentation #.(format nil "A standard implementation of Gateway protocol ~
class ENGINE.

This engine contains a lparallel kernel. The kernel workers each call the ~
handler function on the connection and message passed to the kernel. That ~
function executes a single iteration of the engine's job and is not meant to ~
loop.")))

(defmethod handler ((engine standard-engine) &optional type)
  (declare (ignore type))
  (slot-value engine '%handler))

(defmethod (setf handler) (new-value (engine standard-engine) &optional type)
  (declare (ignore type))
  (setf (slot-value engine '%handler) new-value))

(define-print (standard-engine stream)
  (if (deadp standard-engine)
      (format stream "(DEAD)")
      (format stream "(~D workers, ALIVE)" (worker-count standard-engine))))

(defun worker-count (engine)
  (check-type engine standard-engine)
  (let ((lparallel:*kernel* (%kernel engine)))
    (lparallel:kernel-worker-count)))

(define-constructor (standard-engine handler threads)
  (assert (functionp handler) () "HANDLER must be a function, not ~S." handler)
  (let* ((threads (or threads
                      ;; (config :engine-threads) ;; TODO
                      (cl-cpus:get-number-of-processors)))
         (name "Gateway - Engine, ~D threads" cpus))
    (v:trace '(:gateway :engine)
             "Standard engine starting with ~D threads." threads)
    (let* ((thread-name (uiop:strcat name " (lparallel kernel)"))
           (kernel (lparallel:make-kernel threads :name thread-name))
           (lparallel:*kernel* kernel))
      (flet ((cleaner-loop ()
               (loop (lparallel:receive-result (channel standard-engine)))))
        (setf (name standard-engine) name
              (%kernel standard-engine) kernel
              (channel standard-engine) (lparallel:make-channel)
              (cleaner standard-engine)
              (bt:make-thread #'cleaner-loop
                              :name "Gateway - Engine cleaner thread"))))))

(defmethod deadp ((engine standard-engine))
  (not (lparallel.kernel::alivep (%kernel engine))))

(defmethod kill ((engine standard-engine))
  (v:trace :gateway "Standard engine was killed.")
  (let ((lparallel:*kernel* (%kernel engine)))
    (lparallel:end-kernel :wait t))
  (bt:destroy-thread (cleaner engine))
  (values))

(defmethod accept-message
    ((engine standard-engine) connection message)
  (lparallel:submit-task (channel engine) (handler engine) connection message)
  t)
