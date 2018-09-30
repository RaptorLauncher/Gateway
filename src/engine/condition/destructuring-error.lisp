;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; engine/impl/destructuring-error.lisp

(in-package #:gateway.engine/condition)

;; TODO test this
(define-condition destructuring-error (error gateway-condition)
  ((%expression :reader destructuring-error-expression
                :initarg :expression
                :initform (error "Must provide EXPRESSION."))
   (%reason :reader destructuring-error-reason
            :initarg :reason
            :initform nil))
  (:report print-destructuring-error))

(defun print-destructuring-error (condition stream)
  (format stream "Failed to destructure ~S~:[.~;:~%~:*~A~]"
          (destructuring-error-expression condition)
          (destructuring-error-reason condition)))

(defmethod data-object-using-class
    ((class (eql (find-class 'destructuring-error))) data)
  (destructuring-bind (expression &optional reason) data
    (make-instance class :expression expression :reason reason)))

(defmethod object-data ((condition destructuring-error))
  (list* (class-name (class-of condition))
         (destructuring-error-expression condition)
         (let ((reason (destructuring-error-reason condition)))
           (typecase reason
             (condition (list (princ-to-string reason)))
             (string (list reason))
             (null '())))))

;;; DATA-OBJECT implementation

;; TODO test this
;; TODO make the test fail unless it is tested with all gateway conditions
(defmethod data-object ((data cons))
  (handler-case
      (destructuring-bind (object-type . body) data
        (let* ((class (or (string-subclassp object-type 'gateway-object)
                          (string-subclassp object-type 'gateway-condition))))
          (assert (not (null class)))
          (data-object-using-class class body)))
    (destructuring-bind-star:destructuring-error (e)
      (error (make-instance
              'destructuring-error
              :expression
              (destructuring-bind-star:destructuring-error-expression e)
              :reason
              (destructuring-bind-star:destructuring-error-reason e))))))

(defun string-subclassp (string-designator superclass)
  (when (symbolp superclass) (setf superclass (find-class superclass)))
  (find string-designator (subclasses superclass) :test #'string=
                                                  :key #'class-name))
