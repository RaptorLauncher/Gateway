;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; engine/impl/data-object.lisp

(in-package #:gateway.engine/impl)

;; TODO tests
;; TODO logging
(defmethod data-object ((data cons))
  (multiple-value-bind (class body extra-data) (%data-object data)
    (handler-case
        (apply #'data-object-using-class class body extra-data)
      (error (e) (read-error 'invalid-message-body data e)))))

(defun %data-object (data &optional messagep)
  (handler-case
      (destructuring-bind (object-type . body) data
        (typecase object-type
          ((cons t (cons t null))
           (unless (and (symbolp (first object-type))
                        (member (first object-type) '(:c :s) :test #'string=)
                        (typep (second object-type) 'unsigned-byte))
             (read-error 'invalid-message-id object-type))
           (if messagep
               (read-error 'message-read-error
                           object-type "Duplicate message IDs")
               (multiple-value-bind (class body) (%data-object (cdr data) t)
                 (values class body (list :id object-type)))))
          (symbol
           (let* ((class (or (cable-subclass object-type 'gateway-object)
                             (cable-subclass object-type 'gateway-condition))))
             (unless (not (null class))
               (read-error 'invalid-message-type object-type
                           "Object class ~S was not found." object-type))
             (unless (not (protocol-object-p class))
               (read-error 'invalid-message-type object-type
                           "Object class ~S is a protocol class and can't be ~
                            instantiated." object-type))
             (values class body nil)))
          (t (read-error 'object-read-error data
                         "Invalid message class: ~A" object-type))))
    ((and error (not read-error)) (e)
      (error (read-error 'object-read-error data e)))))

(defun cable-subclass (class-name superclass)
  (when (symbolp superclass) (setf superclass (find-class superclass)))
  (find class-name (subclasses superclass) :test #'cable-equal
                                           :key #'class-name))

(defun class-equal (x y)
  (let ((two-symbol-list '(cons symbol (cons symbol null))))
    (or (and (symbolp x) (symbolp y)
             (find-symbol (symbol-name x) :keyword)
             (string= x y))
        (and (typep x two-symbol-list)
             (typep y two-symbol-list)
             (find-symbol (symbol-name (first x)) :keyword)
             (string= (first x) (first y))
             (string= (second x) (second y)))
        (equal x y))))

;; TODO support the PROXY protocol in connector
