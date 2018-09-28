;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/messages/standard-message.lisp

(in-package #:gateway.engine/messages)

(defclass standard-message (message)
  ((%body :reader body
          :initarg :body
          :initform '())
   (%status :reader status
            :initarg :status
            :initform nil)))

(define-constructor (standard-message status)
  (check-type status (member :request :ok :error)))

(define-print (standard-message stream :type nil)
  (let ((name (class-name (class-of standard-message)))
        (status (status standard-message)))
    (if (eq :request status)
        (prin1 name stream)
        (format stream "(~A ~S)" status name))))
