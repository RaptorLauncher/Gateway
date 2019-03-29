;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; base/impl/data-object.lisp

(in-package #:gateway.base/impl)

(defmethod data-object ((data cons))
  (flet ((parse (data)
           (handler-case
               (destructuring-bind (object-type . body) data
                 (%data-object object-type body))
             ((and error (not read-error)) (e)
               (error (read-error 'object-read-error data e))))))
    (multiple-value-bind (class body extra-data) (parse data)
      (handler-case
          (apply #'data-object-using-class class body extra-data)
        ((and error (not read-error)) (e)
          (read-error 'invalid-message-body data e))))))

(defgeneric %data-object (object-type body &optional messagep)
  (:method ((object-type cons) body &optional messagep)
    (when messagep
      (read-error 'message-read-error object-type "Duplicate message IDs"))
    (unless (and (symbolp (first object-type))
                 (member (first object-type) '(:c :s) :test #'string=)
                 (typep (second object-type) 'unsigned-byte))
      (read-error 'invalid-message-id object-type))
    (multiple-value-bind (class body) (%data-object (car body) (cdr body) t)
      (values class body (list :id object-type))))
  (:method ((object-type symbol) body &optional messagep)
    (declare (ignore messagep))
    (let* ((class (or (cable-subclass object-type 'gateway-object)
                      (cable-subclass object-type 'gateway-condition))))
      (when (null class)
        (read-error 'invalid-message-class object-type
                    "Object class ~S was not found." object-type))
      (when (protocol-object-p class)
        (let* ((name (uiop:strcat "STANDARD-" (symbol-name (class-name class))))
               (subclasses (remove-if-not #'symbolp (subclasses class)
                                          :key #'class-name))
               (subclass (find name subclasses :key #'class-name)))
          (unless subclass
            (read-error 'invalid-message-class object-type
                        "Gateway bug: class ~A was not found." name))
          (setf class subclass)))
      (values class body nil)))
  (:method (object-type body &optional messagep)
    (declare (ignore messagep))
    (read-error 'invalid-message-class object-type)))

(defun cable-subclass (class-name superclass)
  (check-type class-name (or class symbol))
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
