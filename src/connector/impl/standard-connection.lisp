;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/impl/standard-connection.lisp

(in-package :gateway.connector)

(defclass standard-connection (connection usocket:stream-usocket)
  ((%address :accessor address))
  (:documentation #.(format nil "A standard implementation of Gateway protocol ~
class CONNECTION. This class is a subclass of USOCKET:STREAM-SOCKET and may be ~
instantiated in one of two ways: using MAKE-INSTANCE on it, or invoking
CHANGE-CLASS on an instance of USOCKET:STREAM-SOCKET.")))

(define-print (standard-connection stream)
  (format stream "~A (~A)" (address standard-connection)
          (if (deadp standard-connection) "DEAD" "ALIVE")))

(defun %initialize-connection (connection)
  (let ((address (socket-peer-address connection)))
    (v:trace '(:gateway :connection)
             "Standard connection created for ~A." address)
    (setf (address connection) address)))

(define-constructor (standard-connection)
  (%initialize-connection standard-connection))

(defmethod update-instance-for-different-class
    ((socket usocket:stream-usocket) (connection connection) &key)
  (call-next-method)
  (%initialize-connection connection))

(defmethod deadp ((connection standard-connection))
  (handler-case (peek-char-no-hang (usocket:socket-stream connection))
    (error () (usocket:socket-close connection)))
  (not (open-stream-p (usocket:socket-stream connection))))

(defmethod kill ((connection standard-connection))
  (usocket:socket-close connection)
  (v:trace '(:gateway :connection)
           "Standard connection killed for ~A." (address connection))
  (values))

(defun connection-readyp (connection)
  (handler-case (peek-char-no-hang (usocket:socket-stream connection))
    (end-of-file () t)))

(defmethod readyp ((connection standard-connection))
  (or (not (open-stream-p (usocket:socket-stream connection)))
      (connection-readyp connection)))

(defmethod connection-receive ((connection standard-connection))
  (cond ((deadp connection) (values nil nil))
        ((not (connection-readyp connection)) (values nil t))
        (t (let ((result (from-cable (usocket:socket-stream connection))))
             (v:trace '(:gateway :connection)
                      "Received ~S from ~A." result connection)
             (values result t)))))

(defmethod connection-send ((connection standard-connection) object)
  (let ((stream (usocket:socket-stream connection)))
    (v:trace '(:gateway :connection)
             "Sending ~S to ~A." object connection)
    (to-cable object stream)
    (terpri stream)
    (force-output stream)))

(defun %ready (connections)
  (usocket:wait-for-input connections :timeout 0.1 :ready-only t))

(defmethod ready-connection-using-class
    ((class (eql (find-class 'standard-connection))) connections)
  (let ((connections connections))
    (loop
      (handler-case
          (let* ((connection (first (%ready connections))))
            (v:trace '(:gateway :connection)
                     "Connection ~A is ready." connection)
            (return (values connection connections)))
        ((or stream-error usocket:socket-error) ()
          (setf connections (remove-if #'deadp connections))
          ;; TODO make use of secondary value in listener
          (unless connections (return (values nil nil))))))))
