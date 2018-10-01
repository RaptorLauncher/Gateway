;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; engine/impl/data-message.lisp

(in-package #:gateway.engine/impl)

;; TODO tests for ALL of these
;; TODO logging everywhere
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
      (read-error 'message-read-error data e))))

(defun check-id-valid (id)
  (handler-case
      (destructuring-bind (owner number) id
        (assert (member owner '(:c :s) :test #'cable-equal) ()
                "The first part of the ID should be C or S, not ~S." owner)
        (assert (typep number 'unsigned-byte) ()
                "The second part of the ID should be a number, not ~S." number))
    (error (e)
      (error (read-error 'invalid-message-id id e)))))

(defun check-message-type-valid (message-type)
  (let ((type '(or symbol (cons symbol (cons symbol null)))))
    (handler-case
        (progn (assert (typep message-type type) ()
                       "Message type should be a symbol or a two-element list, ~
                        not ~S." message-type)
               (when (listp message-type)
                 (let ((status (first message-type)))
                   (assert (member status '(:ok :error) :test #'cable-equal) ()
                           "The message status should be OK or ERROR, not ~S."
                           status))))
      (error (e)
        (read-error 'invalid-message-type message-type e)))))

(defun message-type-class (message-type)
  (handler-case
      (let* ((subclasses (subclasses (find-class 'standard-message)))
             (class (find message-type subclasses :test #'string=
                                                  :key #'class-name)))
        (assert (not (null class)) ()
                "Message type ~S was not found." message-type)
        (assert (not (protocol-object-p class)) ()
                "Message type ~S is a protocol message type and can't be ~
                 instantiated." message-type)
        class)
    (error (e)
      (read-error 'invalid-message-type message-type e))))

(defun parse-message-type (message-type)
  (cond ((atom message-type) (values message-type :request))
        ((cable-equal (first message-type) :ok)
         (values (second message-type) :ok))
        ((cable-equal (first message-type) :error)
         (values (second message-type) :error))
        (t (read-error 'invalid-message-type message-type))))

;; TODO support the PROXY protocol
