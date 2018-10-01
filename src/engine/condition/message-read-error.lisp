;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; engine/condition/message-read-error.lisp

(in-package #:gateway.engine/condition)

;; TODO test this
(define-condition message-read-error (gateway-condition destructuring-error)
  (;; FIXME SBCL screws up DEFINE-CONDITION with :ALLOCATION :CLASS and
   ;; :DEFAULT-INITARGS. Use string-returning methods as a workaround for that.
   ;; (%format-control :reader format-control
   ;;                  :initarg :format-control
   ;;                  :allocation :class)
   )
  ;; (:default-initargs
  ;;  :format-control "Failed to read message ~S~:[.~;:~%~:*~A~]")
  (:report (lambda (c s)
             (format s (format-control c) (expression c) (reason c)))))

(defgeneric format-control (condition))

(defmethod format-control :around (condition)
  (concatenate 'string (call-next-method) " ~S~:[.~;:~%~:*~A~]"))

(defmethod format-control ((object message-read-error)) ()
  "Failed to read message")

(defmethod data-object-using-class
    ((class (eql (find-class 'message-read-error))) data)
  (destructuring-bind (expression &optional reason) data
    (make-instance class :expression expression :reason reason)))

(defmethod object-data ((condition message-read-error))
  (list* (class-name (class-of condition))
         (expression condition)
         (when (reason condition)
           (list (princ-to-string (reason condition))))))

(defun message-read-error (condition-type expression &optional reason)
  (error (make-instance condition-type :expression expression :reason reason)))

(define-condition invalid-message-id (message-read-error) ())

;; FIXME same bug as above.
(defmethod format-control ((object invalid-message-id)) ()
  "Invalid message ID")

(define-condition invalid-message-type (message-read-error) ())

;; FIXME same bug as above.
(defmethod format-control ((object invalid-message-type)) ()
  "Invalid message type")

(define-condition invalid-message-body (message-read-error) ())

;; FIXME same bug as above.
(defmethod format-control ((object invalid-message-body)) ()
  "Invalid message body")
