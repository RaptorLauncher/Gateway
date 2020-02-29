;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2020
;;;; client/client.lisp

(defpackage #:gateway.client
  (:use #:cl)
  (:local-nicknames (#:l #:gateway.library)
                    (#:φ #:phoe-toolbox)
                    (#:z #:pzmq)))

(in-package #:gateway.client)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Client networking

(defparameter *client-context* nil)
(defparameter *client-socket* nil)
(defparameter *client-socket-options*
  '(:linger 100
    :probe-router 1))

(defun start-client (&optional (server-address "tcp://localhost:6500"))
  (setf *client-context* (z:ctx-new))
  (setf *client-socket* (z:socket *client-context* :dealer))
  (apply #'l:set-socket-options *client-socket* *client-socket-options*)
  (z:connect *client-socket* server-address))

(defun stop-client ()
  (l:kill-socket *client-socket*)
  (l:kill-context *client-context*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Client logic

(defun client-send (data)
  (φ:fformat t "~&Sending data ~S." data)
  (z:send *client-socket*
          (with-output-to-string (stream)
            (gateway.cable:to-cable data stream))))

(defun client-receive ()
  (loop for message
          = (handler-case (z:recv-string *client-socket* :dontwait t)
              (z:eagain () nil))
        while message
        do (φ:fformat t "~&Received data ~S." message)
        collect (with-input-from-string (stream message)
                  (gateway.cable:from-cable stream))))
