;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; base/impl/standard-message.lisp

(in-package #:gateway.base/impl)

;;; STANDARD-MESSAGE

(define-protocol-class standard-message (message)
  ((%id :reader id
        :initarg :id)))

(define-constructor (standard-message id)
  (let ((type '(cons (member :client :server) (cons unsigned-byte null))))
    (assert (typep id type) () "Invalid message ID: ~S" id)))

(define-print (standard-message stream :type nil)
  (princ (class-name (class-of standard-message)) stream)
  (when (slot-boundp standard-message '%id)
    (destructuring-bind (owner number) (id standard-message)
      (let ((owner (ecase owner (:server :s) (:client :c))))
        (format stream " (~A ~A)" owner number)))))

(define-protocol-class (:request) (standard-message) ())

(define-protocol-class (:response) (standard-message) ())

(define-protocol-class (:error) (standard-message)
  ((%reason :accessor reason
            :initarg :reason
            :initform nil)))
