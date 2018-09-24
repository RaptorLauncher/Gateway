;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/impl/standard-acceptor.lisp

(in-package :gateway.connector)

(defclass standard-acceptor (acceptor)
  ((%socket :accessor socket-of)
   (%thread :accessor thread)
   (%name :accessor name)
   (%address :accessor address)
   (%handler :accessor handler
             :initarg :handler
             :initform (error "Must define a handler function.")))
  (:documentation #.(format nil "A standard acceptor implementation, with a ~
single server socket. Whenever a socket connection is initiated from outside, ~
a connection is created and the handler function is called with it as an ~
argument.")))

(define-print (standard-acceptor stream)
  (format stream "~A (~:[ALIVE~;DEAD~])" (address standard-acceptor)
          (deadp standard-acceptor)))

(define-constructor (standard-acceptor (host "127.0.0.1") (port 0))
  (check-type host string)
  (check-type port (unsigned-byte 16))
  (let* ((socket (usocket:socket-listen "127.0.0.1" port :reuseaddress t))
         (address (socket-local-address socket))
         (name (format nil "Gateway - Acceptor for ~A" address))
         (fn (curry #'acceptor-loop standard-acceptor)))
    (setf (socket-of standard-acceptor) socket
          (address standard-acceptor) address
          (name standard-acceptor) name
          (thread standard-acceptor) (bt:make-thread fn :name name)))
  (v:trace '(:gateway :acceptor) "~A: starting." standard-acceptor))

(defun acceptor-loop (acceptor)
  (labels
      ((accept (socket)
         (loop
           (unless (server-socket-alive-p socket)
             (v:trace '(:gateway :acceptor)
                      "~A: quitting." acceptor)
             (return-from acceptor-loop))
           (when (usocket:wait-for-input socket :timeout 0.1 :ready-only t)
             (return (usocket:socket-accept socket))))))
    (let ((socket (socket-of acceptor)))
      (with-restartability (acceptor)
        (loop
          (let* ((connection (accept socket)))
            (change-class connection'standard-connection)
            (v:debug '(:gateway :acceptor)
                     "Accepting from ~A."
                     (socket-peer-address connection))
            (funcall (handler acceptor) connection)))))))

(defmethod deadp ((acceptor standard-acceptor))
  (not (bt:thread-alive-p (thread acceptor))))

(defmethod kill ((acceptor standard-acceptor))
  (v:trace '(:gateway :acceptor) "~A: killed." acceptor)
  (usocket:socket-close (socket-of acceptor))
  (values))
