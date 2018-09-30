;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; engine/impl/destructuring-error.lisp

(in-package #:gateway.engine/condition)

;; TODO test this
(define-condition gateway-destructuring-error
    (gateway-condition destructuring-error) ())

(defmethod data-object-using-class
    ((class (eql (find-class 'gateway-destructuring-error))) data)
  (destructuring-bind (expression &optional reason) data
    (make-instance class :expression expression :reason reason)))

(defmethod object-data ((condition gateway-destructuring-error))
  (list* (class-name (class-of condition))
         (expression condition)
         (when (reason condition)
           (list (princ-to-string (reason condition))))))

;;; DATA-OBJECT implementation

;; TODO test this
;; TODO make the test fail unless it is run on all concrete Gateway
;;      objects/conditions
(defmethod data-object ((data cons))
  (handler-case
      (destructuring-bind (object-type . body) data
        (let* ((class (or (string-subclassp object-type 'gateway-object)
                          (string-subclassp object-type 'gateway-condition))))
          (assert (not (null class)))
          (data-object-using-class class body)))
    (destructuring-error (e)
      (error (make-instance 'gateway-destructuring-error
                            :expression (expression e)
                            :reason (reason e))))))

(defun string-subclassp (string-designator superclass)
  (when (symbolp superclass) (setf superclass (find-class superclass)))
  (find string-designator (subclasses superclass) :test #'string=
                                                  :key #'class-name))
