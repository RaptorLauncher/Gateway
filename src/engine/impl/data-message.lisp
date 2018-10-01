;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; engine/impl/data-message.lisp

(in-package #:gateway.engine/impl)

(defmethod data-message ((data cons))
  (handler-case
      (destructuring-bind (id message-type . body) data
        (check-id-valid id)
        (check-message-type-valid message-type)
        (multiple-value-bind (message-type status)
            (parse-message-type message-type)
          (let ((class (message-type-class message-type)))
            (data-message-using-class class id status body))))
    (error (e)
      (message-read-error 'message-read-error data e))))

(defun check-id-valid (id)
  (handler-case
      (destructuring-bind (owner number) id
        (assert (member owner '(:c :s) :test #'cable-equal))
        (assert (typep number 'unsigned-byte)))
    (error (e) ;; TODO logging everywhere
      (error (message-read-error 'invalid-message-id id e)))))

(defun check-message-type-valid (message-type)
  (let ((type '(or symbol (cons symbol (cons symbol null)))))
    (handler-case
        (progn (assert (typep message-type type))
               (when (listp message-type)
                 (let ((status (first message-type)))
                   (assert (or (cable-equal status :ok)
                               (cable-equal status :error))))))
      (error (e)
        (message-read-error 'invalid-message-type message-type e)))))

(defun message-type-class (message-type)
  (handler-case
      (let* ((subclasses (subclasses (find-class 'standard-message)))
             (class (find message-type subclasses :test #'string=
                                                  :key #'class-name)))
        (assert (not (null class)))
        (assert (not (protocol-object-p class)))
        class)
    (error (e)
      (message-read-error 'invalid-message-type message-type e))))

(defun parse-message-type (message-type)
  (cond ((atom message-type) (values message-type :request))
        ((cable-equal (first message-type) :ok)
         (values (second message-type) :ok))
        ((cable-equal (first message-type) :error)
         (values (second message-type) :error))
        (t (message-read-error 'invalid-message-type message-type))))

;; TODO support the PROXY protocol
