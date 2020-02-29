;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2020
;;;; server/server.lisp

(defpackage #:gateway.server
  (:use #:cl)
  (:local-nicknames (#:l #:gateway.library)
                    (#:φ #:phoe-toolbox)
                    (#:i #:ironclad)
                    (#:z #:pzmq)
                    (#:t #:local-time)))

(in-package #:gateway.server)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Server networking

(defparameter *server-context* nil)
(defparameter *server-socket* nil)
(defparameter *server-socket-options*
  '(:immediate 1
    :router-handover 1
    :heartbeat-ivl 10000
    :connect-timeout 30000
    :heartbeat-timeout 30000))

(defun start-server (&optional (listen-address "tcp://*:6500"))
  (setf *server-context* (z:ctx-new))
  (setf *server-socket* (z:socket *server-context* :router))
  (apply #'l:set-socket-options *server-socket* *server-socket-options*)
  (z:bind *server-socket* listen-address))

(defun stop-server ()
  (l:kill-socket *server-socket*)
  (l:kill-context *server-context*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Server logic

(defvar *identities-lock* (bt:make-lock))
(defvar *identities* (make-hash-table))

(defun send-message-all-around (string)
  (bt:with-lock-held (*identities-lock*)
    (loop for integer being the hash-key of *identities*
          for identity = (i:integer-to-octets integer :n-bits 40)
          do (φ:fformat t "~&Sending to identity ~S." identity)
             (cffi:with-pointer-to-vector-data (pointer identity)
               (z:send *server-socket* pointer :len 5 :sndmore t))
             (z:send *server-socket* string))))

(defun echo-server ()
  (loop
    (z:with-message message
      ;; Receive/send identity
      (z:msg-recv message *server-socket*)
      (let* ((identity (cffi:foreign-array-to-lisp
                        (z:msg-data message) `(:array :unsigned-char 5)
                        :element-type '(unsigned-byte 8)))
             (integer (i:octets-to-integer identity :n-bits 40)))
        (bt:with-lock-held (*identities-lock*)
          (cond ((gethash integer *identities*)
                 (φ:fformat t "~&Receiving from identity ~S." identity))
                (t
                 (φ:fformat t "~&Adding identity ~S." identity)
                 (send-message-all-around
                  (prin1-to-string '(message "Someone new just arrived!")))))
          (setf (gethash integer *identities*) (t:now))))
      ;; Receive/send data
      (z:msg-recv message *server-socket*)
      (let ((string (cffi:foreign-string-to-lisp
                     (z:msg-data message)
                     :count (z:msg-size message))))
        (when (string/= string "")
          (φ:fformat t "~&Received data ~S." string)
          (send-message-all-around string))))))
