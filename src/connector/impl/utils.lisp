;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/impl/utils.lisp

(in-package #:gateway.connector)

(defun socket-local-address (socket)
  "Returns a the socket's local address, in format A.B.C.D:E."
  (handler-case
      (format nil "~{~D.~D.~D.~D~}:~D"
              (coerce (usocket:get-local-name socket) 'list)
              (usocket:get-local-port socket))
    (error (e)
      (v:debug '(:gateway :socket) "Socket error: ~A" e)
      (usocket:socket-close socket) "<error>")))

(defun socket-peer-address (socket)
  "Returns a the socket's peer address, in format A.B.C.D:E."
  (handler-case
      (format nil "~{~D.~D.~D.~D~}:~D"
              (coerce (usocket:get-peer-address socket) 'list)
              (usocket:get-peer-port socket))
    (error (e)
      (v:debug '(:gateway :socket) "Socket error: ~A" e)
      (usocket:socket-close socket) "<error>")))

(defun make-connection-pair ()
  (let* ((socket-listen (usocket:socket-listen "127.0.0.1" 0))
         (port (usocket:get-local-port socket-listen))
         (socket-connect (usocket:socket-connect "127.0.0.1" port))
         (socket-accept (usocket:socket-accept socket-listen)))
    (usocket:socket-close socket-listen)
    (list (change-class socket-connect 'standard-connection)
          (change-class socket-accept'standard-connection))))
