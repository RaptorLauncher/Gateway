;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; engine/impl/data-object.lisp

(in-package #:gateway.engine/impl)

(defmethod data-object ((data cons))
  (let (found-class found-body)
    (handler-case
        (destructuring-bind (object-type . body) data
          (let* ((class (or (string-subclassp object-type 'gateway-object)
                            (string-subclassp object-type 'gateway-condition))))
            (assert (not (null class)) ()
                    "Object class ~S was not found." object-type)
            (assert (not (protocol-object-p class)) ()
                    "Object class ~S is a protocol class and can't be ~
                     instantiated." object-type)
            (setf found-class class
                  found-body body)))
      (error (e)
        (error (read-error 'object-read-error data e))))
    (data-object-using-class found-class found-body)))

(defun string-subclassp (string-designator superclass)
  (when (symbolp superclass) (setf superclass (find-class superclass)))
  (find string-designator (subclasses superclass) :test #'string=
                                                  :key #'class-name))
