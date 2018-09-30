;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/impl/standard-message.lisp

(in-package #:gateway.engine/impl)

(defclass standard-message (message)
  ((%id :reader id
        :initarg :id)
   (%status :reader status
            :initarg :status
            :initform :request)
   (%condition :reader condition-of
               :initarg :condition
               :initform nil)))

(define-constructor (standard-message id status)
  (check-type id (cons (member :client :server) (cons unsigned-byte null)))
  (check-type status (member nil :request :ok :error))
  (when (eq (class-of standard-message) (find-class 'standard-message))
    (warn "Creating a direct instance of STANDARD-MESSAGE.")))

(define-print (standard-message stream :type nil)
  (let ((name (class-name (class-of standard-message)))
        (status (status standard-message)))
    (if (eq status :request)
        (prin1 name stream)
        (format stream "(~A ~S)" status name))
    (destructuring-bind (owner number) (id standard-message)
      (let ((owner (ecase owner (:server :s) (:client :c))))
        (format stream " (~A ~A)" owner number)))))

(defmethod data-message ((data cons))
  (destructuring-bind (id message-type . body) data
    (destructuring-bind (owner number) id
      (assert (member owner '(:c :s) :test #'cable-equal))
      ;; TODO add signaling proper condition types here
      (check-type number unsigned-byte))
    (check-type message-type (or symbol (cons symbol (cons symbol null))))
    (let* ((status (message-type-status message-type))
           (message-type (ensure-car message-type))
           (superclass (find-class 'gateway-condition))
           (names (mapcar #'class-name (subclasses superclass)))
           (class (find message-type names :test #'string=)))
      (assert (not (null class)))
      (make-instance class :id id :status status :body body))))

(defun message-type-status (message-type)
  (cond ((atom message-type) :request)
        ((cable-equal (first message-type) :ok) :ok)
        ((cable-equal (first message-type) :error) :error)
        (t (error "Invalid status"))))

;; TODO support the PROXY protocol
