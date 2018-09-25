;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/impl/standard-acceptor.lisp

(in-package :gateway.connector)

(defclass standard-acceptor (acceptor)
  ((%socket :accessor socket-of)
   (%thread :accessor thread)
   (%name :reader name)
   (%timeout :accessor timeout
             :initarg :timeout
             :initform 0.01)
   (%hostname :reader hostname
              :initarg :hostname
              :initform "127.0.0.1")
   (%port :reader port
          :initarg :port
          :initform 0)
   (%queue :reader queue
           :initform (lparallel.queue:make-queue))
   (%handler :reader handler
             :initarg :handler
             :initform (error "Must define a handler function.")))
  (:documentation #.(format nil "A standard acceptor implementation, with a ~
single server socket. Whenever a socket connection is initiated from outside, ~
a connection is created and the handler function is called with it as an ~
argument.")))

(define-print (standard-acceptor stream)
  (format stream "~A (~:[ALIVE~;DEAD~])" (address standard-acceptor)
          (deadp standard-acceptor)))

(define-constructor (standard-acceptor (port 0))
  (check-type port (unsigned-byte 16))
  (let* ((host (hostname standard-acceptor))
         (socket (usocket:socket-listen host port :reuseaddress t)))
    (when (= 0 port)
      (setf (slot-value standard-acceptor '%port)
            (usocket:get-local-port socket)))
    (let* ((address (address standard-acceptor))
           (name (format nil "Gateway - Acceptor for ~A" address))
           (fn (curry #'acceptor-loop standard-acceptor)))
      (setf (socket-of standard-acceptor) socket
            (slot-value standard-acceptor '%name) name
            (thread standard-acceptor) (bt:make-thread fn :name name))))
  (v:trace '(:gateway :acceptor) "~A: starting." standard-acceptor))

(defun acceptor-loop (acceptor)
  (let ((timeout (timeout acceptor)))
    (labels ((accept (socket)
               (loop
                 (when (lparallel.queue:try-pop-queue (queue acceptor))
                   (v:trace '(:gateway :acceptor) "~A: quitting." acceptor)
                   (usocket:socket-close socket)
                   (return-from acceptor-loop))
                 (when (wait-for-sockets socket timeout)
                   (return (usocket:socket-accept socket))))))
      (let ((socket (socket-of acceptor)))
        (with-restartability ()
          (loop
            (let* ((connection (accept socket)))
              (change-class connection'standard-connection)
              (v:debug '(:gateway :acceptor) "Accepting from ~A."
                       (socket-peer-address connection))
              (funcall (handler acceptor) connection))))))))

(defmethod deadp ((acceptor standard-acceptor))
  (not (bt:thread-alive-p (thread acceptor))))

(defmethod kill ((acceptor standard-acceptor))
  (v:trace '(:gateway :acceptor) "~A: killed." acceptor)
  (lparallel.queue:push-queue t (queue acceptor))
  (values))
