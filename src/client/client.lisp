;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2020
;;;; client/client.lisp

(defpackage #:gateway.client
  (:use #:cl)
  (:local-nicknames (#:l #:gateway.library)
                    (#:i #:ironclad)
                    (#:φ #:phoe-toolbox)
                    (#:z #:pzmq)
                    (#:b #:babel)
                    (#:c #:gateway.cable)))

(in-package #:gateway.client)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Client networking

(defparameter *client-context* nil)
(defparameter *client-socket* nil)
(defparameter *client-socket-options*
  '(:linger 1000))

(defun start-client (&optional (server-address "tcp://localhost:6500"))
  (setf *client-context* (z:ctx-new))
  (setf *client-socket* (z:socket *client-context* :dealer))
  (apply #'l:set-socket-options *client-socket* *client-socket-options*)
  (setf *random-state* (make-random-state t))
  (let ((identity (concatenate '(vector (unsigned-byte 8))
                               (b:string-to-octets "Gateway Client")
                               '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                               (i:random-data 32))))
    (z:setsockopt *client-socket* :identity identity))
  (z:connect *client-socket* server-address))

(defun stop-client ()
  (l:kill-socket *client-socket*)
  (l:kill-context *client-context*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Client logic

(defvar *client-public-key*)
(defvar *client-private-key*)

(defvar *cipher* nil)

(defun client-handshake ()
  (prepare-client)
  (client-send `(0 :HELLO "Gateway Client" ,(l:gateway-version)))
  (client-receive)
  (client-send `(2 :CRYPTOGRAPHY?))
  (let* ((response (client-receive))
         (iv (i:hex-string-to-byte-array (eighth response)))
         (server-key-string (sixth response))
         (octets (i:hex-string-to-byte-array server-key-string))
         (server-key (i:make-public-key :curve25519 :y octets))
         (shared-key (i:diffie-hellman *client-private-key*
                                       server-key))
         (aes (i:make-cipher :aes :key shared-key
                                  :mode :ctr
                                  :initialization-vector iv))
         (plist (i:destructure-public-key *client-public-key*))
         (key (i:byte-array-to-hex-string (getf plist :y))))
    (client-send `(4 :CRYPTOGRAPHY :AES-CTR :CURVE25519 :KEY ,key))
    (setf *cipher* aes)
    (print (client-receive))))

(defun prepare-client ()
  (unless (and (boundp '*client-private-key*) (boundp '*client-public-key*))
    (setf (values *client-private-key* *client-public-key*)
          (i:generate-key-pair :curve25519)))
  (unless (and *client-context* *client-socket*) (start-client)))

(defun client-receive ()
  (funcall (if *cipher* #'client-receive-aes #'client-receive-plaintext)))

(defun client-send (data)
  (funcall (if *cipher* #'client-send-aes #'client-send-plaintext) data))

(defun client-receive-plaintext (&optional dontwait)
  (let ((string (handler-case (z:recv-string *client-socket* :dontwait dontwait)
                  (z:eagain () (return-from client-receive-plaintext)))))
    (φ:fformat t "~&Received data ~A." string)
    (with-input-from-string (stream string)
      (gateway.cable:from-cable stream))))

(defun client-receive-aes (&optional dontwait)
  (flet ((message-octets (message)
           (let ((type `(:array :unsigned-char ,(z:msg-size message))))
             (cffi:foreign-array-to-lisp (z:msg-data message) type
                                         :element-type '(unsigned-byte 8)))))
    (z:with-message message
      (z:msg-recv message *client-socket* :dontwait dontwait)
      (let* ((octets (message-octets message))
             (decrypted (i:decrypt-message *cipher* octets))
             (string (b:octets-to-string decrypted)))
        (when (string/= string "")
          (φ:fformat t "~&Received data ~A." string)
          (handler-case
              (with-input-from-string (stream string)
                (c:from-cable stream))
            (error (e)
              (φ:fformat t "~&Dropping malformed message ~S:~%~A"
                         string e))))))))

(defun client-send-plaintext (data)
  (φ:fformat t "~&Sending data ~S." data)
  (z:send *client-socket*
          (with-output-to-string (stream)
            (gateway.cable:to-cable data stream))))

(defun client-send-aes (data)
  (flet ((round-up-to-8 (x) (* (ceiling x 8) 8)))
    (let ((string (with-output-to-string (stream) (c:to-cable data stream)))
          (*print-right-margin* most-positive-fixnum))
      (φ:fformat t "~&Sending data ~S." data)
      (let* ((octets (b:string-to-octets string))
             (new-length (round-up-to-8 (length octets)))
             (new-octets (make-array new-length
                                     :element-type '(unsigned-byte 8)
                                     :initial-element (char-code #\Space))))
        (setf (subseq new-octets 0) octets)
        (cffi:with-pointer-to-vector-data
            (pointer (i:encrypt-message *cipher* new-octets))
          (z:send *client-socket* pointer :len new-length))))))
