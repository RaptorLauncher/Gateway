;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/impl/standard-message.lisp

(in-package #:gateway.engine/impl)

(define-protocol-class standard-message (message)
  ((%id :reader id
        :initarg :id
        :initform nil)
   (%status :reader status
            :initarg :status
            :initform :request)))

(define-constructor (standard-message id status)
  (assert (typep id '(cons (member :client :server) (cons unsigned-byte null))))
  (assert (typep status '(member nil :request :ok :error))))

(define-print (standard-message stream :type nil)
  (let ((name (class-name (class-of standard-message))))
    (if (and (slot-boundp standard-message '%id)
             (slot-boundp standard-message '%status))
        (let ((id (id standard-message))
              (status (status standard-message)))
          (if (eq status :request)
              (prin1 name stream)
              (format stream "(~A ~S)" status name))
          (destructuring-bind (owner number) id
            (let ((owner (ecase owner (:server :s) (:client :c))))
              (format stream " (~A ~A)" owner number))))
        (prin1 name stream))))

;; TODO: data-message-using-class
;; We have separate protocols here; one for messages (for network communication)
;; and one for objects (serializable data). Messages can contain objects.
;; That's why we must have separate serialization protocols.

;; TODO rewrite using data-object-using-class(?)
(defmethod data-message ((data cons))
  ;; (handler-case
  ;;     (destructuring-bind (id message-type . body) data
  ;;       )
  ;;   (destructuring-error (e)))
  (destructuring-bind (id message-type . body) data
    (destructuring-bind (owner number) id
      (assert (member owner '(:c :s) :test #'cable-equal))
      ;; TODO add signaling proper condition types here
      (check-type number unsigned-byte))
    (check-type message-type (or symbol (cons symbol (cons symbol null))))
    (multiple-value-bind (message-type status)
        (parse-message-type message-type)
      (let* ((superclass (find-class 'gateway-condition))
             (names (mapcar #'class-name (subclasses superclass)))
             (class (find message-type names :test #'string=)))
        (assert (not (null class)))
        (assert (not (protocol-object-p class)))
        (make-instance class :id id :status status :body body)))))

(defun parse-message-type (message-type)
  (cond ((atom message-type) (values message-type :request))
        ((cable-equal (first message-type) :ok)
         (values (second message-type) :ok))
        ((cable-equal (first message-type) :error)
         (values (second message-type) :error))
        (t (error "Invalid message type"))))

;; TODO support the PROXY protocol
