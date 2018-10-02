;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/impl/standard-message.lisp

(in-package #:gateway.engine/message)

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
