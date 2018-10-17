;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/message/standard-message.lisp

(in-package #:gateway.engine/message)

;;; STANDARD-MESSAGE

(define-protocol-class standard-message (message)
  ((%id :reader id
        :initarg :id)
   (%body :reader body
          :initarg :body))
  (:default-initargs :body nil))

(define-constructor (standard-message id)
  (let ((type '(cons (member :client :server) (cons unsigned-byte null))))
    (assert (typep id type) () "Invalid message ID: ~S" id)))

(define-print (standard-message stream :type nil)
  (princ (class-name (class-of standard-message)) stream)
  (when (slot-boundp standard-message '%id)
    (destructuring-bind (owner number) (id standard-message)
      (let ((owner (ecase owner (:server :s) (:client :c))))
        (format stream " (~A ~A)" owner number)))))
