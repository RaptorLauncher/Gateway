;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; engine/condition/read-error.lisp

(in-package #:gateway.engine/condition)

(define-protocol-condition-type read-error
    (gateway-condition destructuring-error)
  ((%format-control :reader format-control
                    :initarg :format-control
                    ;; SBCL screws up DEFINE-CONDITION with :ALLOCATION :CLASS
                    ;; and :DEFAULT-INITARGS. TODO uncomment in SBCL 1.4.13+.
                    ;; :allocation :class
                    ))
  (:default-initargs :format-control "Error while reading")
  (:report (lambda (c s)
             (format s "~A ~S~:[.~;:~%~:*~A~]"
                     (format-control c) (expression c) (reason c)))))

(defmethod data-object-using-class
    ((class (eql (find-class 'read-error))) data &key)
  (make-read-error class data))

(defmethod object-data ((condition read-error))
  (list* (class-name (class-of condition))
         (expression condition)
         (when (reason condition)
           (list (princ-to-string (reason condition))))))

(defun make-read-error (class data)
  (destructuring-bind (expression &optional reason) data
    (make-instance class :expression expression :reason reason)))

(defun read-error (condition-type expression &optional reason &rest args)
  (let ((reason (if args (format nil reason args) reason)))
    (error (make-instance condition-type :expression expression
                                         :reason reason))))

(defmacro define-read-error (name superclasses &body (format-control . body))
  (declare (ignore body))
  `(progn
     (define-condition ,name ,superclasses ()
       (:default-initargs :format-control ,format-control))
     (defmethod data-object-using-class
         ((class (eql (find-class ',name))) data &key)
       (make-read-error class data))))

(define-read-error object-read-error (read-error)
  "Failed to read object")

(define-read-error message-read-error (object-read-error)
  "Failed to read message")

(define-read-error invalid-message-id (message-read-error)
  "Invalid message ID")

(define-read-error invalid-message-class (message-read-error)
  "Invalid message class")

(define-read-error invalid-message-body (message-read-error)
  "Invalid message body")
