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
  (format stream "~A (~A)" (address standard-acceptor)
          (if (deadp standard-acceptor) "DEAD" "ALIVE")))

(define-constructor (standard-acceptor (host "127.0.0.1") (port 0))
  (check-type host string)
  (check-type port (unsigned-byte 16))
  (v:trace '(:gateway :acceptor)
           "Standard acceptor starting at ~A:~D." host port)
  (let* ((socket (usocket:socket-listen "127.0.0.1" port :reuseaddress t))
         (address (socket-local-address socket))
         (name (format nil "Gateway - Acceptor for ~A" address))
         (fn (curry #'acceptor-loop standard-acceptor)))
    (setf (socket-of standard-acceptor) socket
          (address standard-acceptor) address
          (name standard-acceptor) name
          (thread standard-acceptor) (bt:make-thread fn :name name))))

(defun acceptor-loop (acceptor)
  (with-restartability (acceptor)
    (loop with socket = (socket-of acceptor)
          for accept = (usocket:socket-accept (usocket:wait-for-input socket))
          for connection = (change-class accept 'standard-connection)
          do (v:debug '(:gateway :acceptor)
                      "Accepting a connection from ~A."
                      (socket-peer-address accept))
             (funcall (handler acceptor) connection))))

(defmethod deadp ((acceptor standard-acceptor))
  (not (bt:thread-alive-p (thread acceptor))))

;; TODO trace KILL, take care of multiple KILL calls
(defmethod kill ((acceptor standard-acceptor))
  (v:trace :gateway "Standard acceptor from ~A was killed." (address acceptor))
  (unless (eq (thread acceptor) (bt:current-thread))
    (bt:destroy-thread (thread acceptor)))
  (unless (deadp acceptor)
    (usocket:socket-close (socket-of acceptor)))
  (values))
