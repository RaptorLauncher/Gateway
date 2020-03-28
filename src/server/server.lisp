;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2020
;;;; server/server.lisp

(defpackage #:gateway.server
  (:use #:cl)
  (:local-nicknames (#:l #:gateway.library)
                    (#:a #:alexandria)
                    (#:φ #:phoe-toolbox)
                    (#:i #:ironclad)
                    (#:z #:pzmq)
                    (#:b #:babel)
                    (#:t #:local-time)
                    (#:c #:gateway.cable)))

(in-package #:gateway.server)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Server networking

(defparameter *server-context* nil)
(defparameter *server-socket* nil)
(defparameter *connection-timeout* (* 2 60 1000))
(defparameter *server-socket-options*
  `(:immediate 1
    :router-handover 1
    :ipv6 t
    :maxmsgsize #.(expt 2 16)
    :heartbeat-ivl 10000
    :connect-timeout ,*connection-timeout*
    :heartbeat-timeout ,*connection-timeout*))

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

(defvar *connections-lock* (bt:make-lock))
(defvar *connections* (make-hash-table :test #'equalp))

(defstruct connection
  identity
  (last-message-time (t:now))
  (cipher nil)
  (iv nil)
  (client-count -2)
  (server-count -1))

(defvar *server-public-key*)
(defvar *server-private-key*)

(defparameter *poll-timeout* 1000)
(defvar *incoming-data* '())
(defvar *outgoing-data* '())

(defun prepare-server ()
  (unless (and (boundp '*server-private-key*) (boundp '*server-public-key*))
    (setf (values *server-private-key* *server-public-key*)
          (i:generate-key-pair :curve25519)))
  (unless (and *server-context* *server-socket*) (start-server)))

(defun server ()
  (prepare-server)
  (unwind-protect
       (z:with-poll-items item (*server-socket*)
         (tagbody
          :receive-incoming
            (pzmq:poll item *poll-timeout*)
            (when (member :pollin (pzmq:revents item 0))
              (receive-incoming *server-socket*)
              (go :receive-incoming))
          :process-incoming
            (dolist (incoming *incoming-data*)
              (destructuring-bind (connection data) incoming
                (process-incoming connection data)))
            (setf *incoming-data* '())
          :send-outgoing
            (dolist (outgoing *outgoing-data*)
              (destructuring-bind (connection data) outgoing
                (send-outgoing *server-socket* connection data)))
            (setf *outgoing-data* '())
          :clean-up-old-connections
            (clean-up-old-connections)
          :loop
            (go :receive-incoming)))
    (stop-server)))

(defun receive-incoming (socket)
  (flet ((message-octets (message)
           (let ((type `(:array :unsigned-char ,(z:msg-size message))))
             (cffi:foreign-array-to-lisp (z:msg-data message) type
                                         :element-type '(unsigned-byte 8)))))
    (z:with-message message
      (z:msg-recv message socket)
      ;; Receive and handle identity.
      (let* ((identity (message-octets message))
             (connection (bt:with-lock-held (*connections-lock*)
                           (gethash identity *connections*))))
        (cond (connection
               (φ:fformat t "~&Receiving from identity ~S." identity)
               (setf (connection-last-message-time connection) (t:now)))
              (t
               (φ:fformat t "~&Adding identity ~S." identity)
               (setf connection (make-connection :identity identity))
               (bt:with-lock-held (*connections-lock*)
                 (setf (gethash identity *connections*) connection))))
        ;; Receive and handle data.
        (z:msg-recv message socket)
        (let* ((cipher (connection-cipher connection))
               (string (if cipher
                           (let* ((octets (message-octets message))
                                  (decrypted (i:decrypt-message cipher octets)))
                             (b:octets-to-string decrypted))
                           (cffi:foreign-string-to-lisp
                            (z:msg-data message) :count (z:msg-size message)))))
          (when (string/= string "")
            (φ:fformat t "~&Received data ~A." string)
            (handler-case
                (with-input-from-string (stream string)
                  (let ((data (c:from-cable stream)))
                    (push (list connection data) *incoming-data*)))
              (error (e)
                (φ:fformat t "~&Dropping malformed message ~S:~%~A"
                           string e)))))))))

(defun process-incoming (connection data)
  (flet ((destructure-data (data)
           (handler-case
               (destructuring-bind (id keyword &rest args) data
                 (assert (typep id 'integer))
                 (assert (typep keyword 'symbol))
                 (unless (= id (+ 2 (connection-client-count connection)))
                   (φ:fformat t "~&Out of order message ~S." data)
                   (return-from process-incoming))
                 (values keyword args))
             (error (e)
               (φ:fformat t "~&Invalid message ~S:~%~A" data e)
               (return-from process-incoming))))
         (send-response (&rest data)
           (let ((counter (incf (connection-client-count connection) 2)))
             (push `(,connection (,counter . ,data)) *outgoing-data*)))
         (send-request (data &optional (connection connection))
           (let ((counter (incf (connection-server-count connection) 2)))
             (push `(,connection (,counter . ,data)) *outgoing-data*))))
    (handler-case
        (multiple-value-bind (keyword data) (destructure-data data)
          (a:switch (keyword :test #'string=)
            (:hello
             (destructuring-bind (client-name client-version) data
               (assert (stringp client-name))
               (assert (stringp client-version))
               (send-response :hello "Gateway Server"
                              (l:gateway-version) (machine-instance))))
            (:ping
             (assert (null data))
             (send-response :pong))
            (:cryptography?
             (assert (null data))
             (let* ((plist (i:destructure-public-key *server-public-key*))
                    (key (i:byte-array-to-hex-string (getf plist :y)))
                    (iv (i:random-data 16))
                    (string-iv (i:byte-array-to-hex-string iv)))
               (setf (connection-iv connection) iv)
               (send-response :ok :aes-ctr :curve25519
                                  :key key :iv string-iv)))
            (:cryptography
             (destructuring-bind (type dh-type key client-key-string) data
               (assert (symbolp type))
               (assert (string= type :aes-ctr))
               (assert (symbolp dh-type))
               (assert (string= dh-type :curve25519))
               (assert (symbolp key))
               (assert (string= key :key))
               (assert (stringp client-key-string))
               (assert (= 64 (array-dimension client-key-string 0)))
               (let* ((octets (i:hex-string-to-byte-array client-key-string))
                      (client-key (i:make-public-key :curve25519 :y octets))
                      (shared-key (i:diffie-hellman *server-private-key*
                                                    client-key))
                      (iv (connection-iv connection))
                      (aes (i:make-cipher :aes :key shared-key
                                               :mode :ctr
                                               :initialization-vector iv)))
                 (setf (connection-cipher connection) aes)
                 (send-response :ok))))
            (:say)
            (t (error "Unknown keyword ~S." keyword))))
      (error (e)
        (φ:fformat t "~&Invalid message ~S:~%~A" data e)
        (return-from process-incoming)))))

(defun send-outgoing (socket connection data)
  (let ((identity (connection-identity connection))
        (*print-right-margin* most-positive-fixnum))
    (φ:fformat t "~&Sending data ~S to identity ~A." data identity)
    (cffi:with-pointer-to-vector-data (pointer identity)
      (z:send socket pointer :len (length identity) :sndmore t)))
  (if (typep data '(vector (unsigned-byte 8)))
      (cffi:with-pointer-to-vector-data (pointer data)
        (z:send socket pointer :len (length data)))
      (flet ((round-up-to-8 (x) (* (ceiling x 8) 8)))
        (let ((string (with-output-to-string (stream) (c:to-cable data stream))))
          (a:if-let ((cipher (connection-cipher connection)))
            (let* ((octets (b:string-to-octets string))
                   (new-length (round-up-to-8 (length octets)))
                   (new-octets (make-array
                                new-length
                                :element-type '(unsigned-byte 8)
                                :initial-element (char-code #\Space))))
              (setf (subseq new-octets 0) octets)
              (cffi:with-pointer-to-vector-data
                  (pointer (i:encrypt-message cipher new-octets))
                (z:send socket pointer :len new-length)))
            (z:send socket string))))))

(defun clean-up-old-connections ()
  (let ((now (t:now)))
    (bt:with-lock-held (*connections-lock*)
      (loop for identity being the hash-key of *connections*
              using (hash-value connection)
            for then = (connection-last-message-time connection)
            when (< *connection-timeout*
                    (* 1000 (t:timestamp-difference now then)))
              do (φ:fformat t "~&Removing stale connection ~S." identity)
              and collect identity into results
            finally (dolist (result results)
                      (remhash result *connections*))))))
