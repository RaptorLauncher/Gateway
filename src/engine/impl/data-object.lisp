;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; engine/impl/data-object.lisp

(in-package #:gateway.engine/impl)

(defmethod data-object ((data cons))
  (handler-case
      (destructuring-bind (object-type . body) data
        (let* ((class (or (string-subclassp object-type 'gateway-object)
                          (string-subclassp object-type 'gateway-condition))))
          (assert (not (null class)))
          (data-object-using-class class body)))
    (destructuring-error (e)
      (error (make-instance 'message-read-error
                            :expression (expression e)
                            :reason (reason e))))))

(defun string-subclassp (string-designator superclass)
  (when (symbolp superclass) (setf superclass (find-class superclass)))
  (find string-designator (subclasses superclass) :test #'string=
                                                  :key #'class-name))
