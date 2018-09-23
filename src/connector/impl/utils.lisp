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
    (error () "<error>")))

(defun socket-peer-address (socket)
  "Returns a the socket's peer address, in format A.B.C.D:E."
  (handler-case
      (format nil "~{~D.~D.~D.~D~}:~D"
              (coerce (usocket:get-peer-address socket) 'list)
              (usocket:get-peer-port socket))
    (error () "<error>")))
