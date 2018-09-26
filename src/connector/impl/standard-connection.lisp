;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/impl/standard-connection.lisp

(in-package :gateway.connector)

(defclass standard-connection (connection usocket:stream-usocket)
  ((%local-address :reader local-address)
   (%peer-address :reader peer-address))
  (:documentation #.(format nil "A standard implementation of Gateway protocol ~
class CONNECTION. This class is a subclass of USOCKET:STREAM-SOCKET and may be ~
instantiated in one of two ways: using MAKE-INSTANCE on it, or invoking
CHANGE-CLASS on an instance of USOCKET:STREAM-SOCKET.")))

(define-print (standard-connection stream)
  (format stream "~A (~:[ALIVE~;DEAD~])" (address standard-connection)
          (deadp standard-connection)))

(defun %initialize-connection (connection)
  (let ((peer-address (socket-peer-address connection))
        (local-address (socket-local-address connection)))
    (setf (slot-value connection' %peer-address) peer-address
          (slot-value connection' %local-address) local-address))
  (v:trace '(:gateway :connection) "~A: created." connection))

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
  (v:trace '(:gateway :connection) "~A: killed." connection)
  (values))

(defun connection-readyp (connection)
  (handler-case (peek-char-no-hang (usocket:socket-stream connection))
    (end-of-file () t)))

(defmethod readyp ((connection standard-connection))
  (or (not (open-stream-p (usocket:socket-stream connection)))
      (connection-readyp connection)))

(defun read-from-cable (stream)
  (handler-case (from-cable-buffered stream)
    (cable-error () nil)))

(defmethod connection-receive ((connection standard-connection))
  (cond ((deadp connection) (values nil nil))
        ((not (connection-readyp connection)) (values nil t))
        (t
         (let ((data (read-from-cable (usocket:socket-stream connection))))
           (v:trace '(:gateway :connection) "~A: got ~S." connection data)
           (values data t)))))

(defmethod connection-send ((connection standard-connection) object)
  (let ((stream (usocket:socket-stream connection)))
    (v:trace '(:gateway :connection)
             "~A: sending ~S." connection object)
    (to-cable object stream)
    (fresh-line stream)
    (force-output stream)))

(defmethod ready-connection-using-class
    ((class (eql (find-class 'standard-connection))) connections
     &key (timeout 0.01))
  (let ((connections connections))
    (loop
      (handler-case
          (let* ((connection (first (wait-for-sockets connections timeout))))
            (when connection
              (v:trace '(:gateway :connection)
                       "~A is ready." connection))
            (return connection))
        (usocket:socket-error (e)
          (v:debug '(:gateway :connection) "Socket error: ~A" e)
          (setf connections (remove-if #'deadp connections)))
        (stream-error (e)
          (let ((connection (find (stream-error-stream e) connections
                                  :key #'usocket:socket-stream)))
            (v:trace '(:gateway :connection)
                     "~A signaled a stream error." connection)
            (return connection)))))))

(defmethod hostname ((connection standard-connection) &optional type)
  (let ((name (ecase type
                (:local (usocket:get-local-name connection))
                ((nil :peer) (usocket:get-peer-name connection)))))
    (format nil "~{~D.~D.~D.~D~}" (coerce name 'list))))

(defmethod port ((connection standard-connection) &optional type)
  (ecase type
    (:local (usocket:get-local-port connection))
    ((nil :peer) (usocket:get-peer-port connection))))

(defmethod address ((connection standard-connection) &optional type)
  (ecase type
    (:local (local-address connection))
    ((nil :peer) (peer-address connection))))
