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
;;; Class definition

(defparameter *default-socket-options*
  `(:linger 1000
    :identity
    ,(let ((name (format nil "Gateway Client ~A" (l:gateway-version))))
       (concatenate '(vector (unsigned-byte 8))
                    (b:string-to-octets name)
                    (make-list (- 64 (length name)) :initial-element 0)
                    (i:random-data 32)))))

(defclass client (l:zmq-socketed l:encrypting) ()
  (:default-initargs
   :socket-type :dealer
   :socket-options *default-socket-options*))

(defmethod start ((client client) &optional (hostname "localhost") (port 6500))
  (let ((address (format nil "tcp://~A:~D" hostname port)))
    (z:connect (l:socket client) address)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Receive

(defun receive (client)
  (let ((string (if (l:cipher client)
                    (receive-aes client)
                    (receive-plaintext client))))
    (when string
      (φ:fformat t "~&Received data ~A." string)
      (handler-case (with-input-from-string (s string) (c:from-cable s))
        (error (e) (φ:fformat t "~&Malformed message ~S:~%~A" string e))))))

(defun receive-plaintext (client &optional dontwait)
  (handler-case (z:recv-string (l:socket client) :dontwait dontwait)
    (z:eagain () (return-from receive-plaintext))))

(defun receive-aes (client &optional dontwait)
  (z:with-message message
    (handler-case (z:msg-recv message (l:socket client) :dontwait dontwait)
      (z:eagain () (return-from receive-aes)))
    (let* ((size (z:msg-size message))
           (octets (cffi:foreign-array-to-lisp
                    (z:msg-data message) `(:array :unsigned-char ,size)
                    :element-type '(unsigned-byte 8)))
           (decrypted (i:decrypt-message (l:cipher client) octets)))
      (b:octets-to-string decrypted))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Send

(defun send (client data)
  (let ((*print-right-margin* most-positive-fixnum))
    (φ:fformat t "~&Sending data ~S." data)
    (if (l:cipher client)
        (send-aes client data)
        (send-plaintext client data))))

(defun send-plaintext (client data)
  (z:send (l:socket client)
          (with-output-to-string (stream)
            (gateway.cable:to-cable data stream))))

(defun send-aes (client data)
  (flet ((round-up-to-8 (x) (* (ceiling x 8) 8)))
    (let ((string (with-output-to-string (stream) (c:to-cable data stream))))
      (let* ((octets (b:string-to-octets string))
             (new-length (round-up-to-8 (length octets)))
             (new-octets (make-array new-length
                                     :element-type '(unsigned-byte 8)
                                     :initial-element (char-code #\Space))))
        (setf (subseq new-octets 0) octets)
        (cffi:with-pointer-to-vector-data
            (pointer (i:encrypt-message (l:cipher client) new-octets))
          (z:send (l:socket client) pointer :len new-length))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Handshake

(defun handshake (client)
  (send client `(0 :HELLO "Gateway Client" ,(l:gateway-version)))
  (receive client)
  (send client `(2 :CRYPTOGRAPHY?))
  (let* ((response (receive client))
         (iv (i:hex-string-to-byte-array (eighth response)))
         (server-key-string (sixth response))
         (octets (i:hex-string-to-byte-array server-key-string))
         (server-key (i:make-public-key :curve25519 :y octets))
         (shared-key (i:diffie-hellman (l:private-key client) server-key))
         (plist (i:destructure-public-key (l:public-key client)))
         (key (i:byte-array-to-hex-string (getf plist :y))))
    (send client `(4 :CRYPTOGRAPHY :AES-CTR :CURVE25519 :KEY ,key))
    (setf (l:cipher client) (i:make-cipher :aes :key shared-key :mode :ctr
                                                :initialization-vector iv)))
  (receive client))
