(ql:quickload '(:pzmq :gateway.cable :local-time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utils

(defmacro kill-socket (place)
  `(when ,place
     (pzmq:close ,place)
     (setf ,place nil)))

(defmacro kill-context (place)
  `(when ,place
     (pzmq:ctx-term ,place)
     (setf ,place nil)))

(defun set-socket-options (socket &rest args)
  (loop for (key value) on args by #'cddr
        do (pzmq:setsockopt socket key value)))

(defun array-to-integer (array)
  (loop with result = 0
        for chunk across array
        do (setf result (+ chunk (ash result 8)))
        finally (return result)))

(defun integer-to-array (integer length)
  (loop repeat length
        with result = '()
        for chunk = (ldb (byte 8 0) integer)
        do (push chunk result) (setf integer (ash integer -8))
        finally (return (coerce result '(vector (unsigned-byte 8))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Client networking

(defparameter *client-context* nil)
(defparameter *client-socket* nil)
(defparameter *client-socket-options*
  '(:linger 100
    :probe-router 1))

(defun start-client (&optional (server-address "tcp://localhost:6500"))
  (setf *client-context* (pzmq:ctx-new))
  (setf *client-socket* (pzmq:socket *client-context* :dealer))
  (apply #'set-socket-options *client-socket* *client-socket-options*)
  (pzmq:connect *client-socket* server-address))

(defun stop-client ()
  (kill-socket *client-socket*)
  (kill-context *client-context*))

;;; Server networking

(defparameter *server-context* nil)
(defparameter *server-socket* nil)
(defparameter *server-socket-options*
  '(:immediate 1
    :router-handover 1
    :heartbeat-ivl 10000
    :connect-timeout 30000
    :heartbeat-timeout 30000))

(defun stop-server ()
  (kill-socket *server-socket*)
  (kill-context *server-context*))

(defun start-server (&optional (listen-address "tcp://*:6500"))
  (setf *server-context* (pzmq:ctx-new))
  (setf *server-socket* (pzmq:socket *server-context* :router))
  (apply #'set-socket-options *server-socket* *server-socket-options*)
  (pzmq:bind *server-socket* listen-address))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Client logic

(defun client-send (data)
  (format t "~&Sending data ~S." data)
  (pzmq:send *client-socket*
             (with-output-to-string (stream) (gateway.cable:to-cable data stream))))

(defun client-receive ()
  (loop for message = (handler-case (pzmq:recv-string *client-socket* :dontwait t)
                        (pzmq:eagain () nil))
        while message
        do (format t "~&Received data ~S." message)
        collect (with-input-from-string (stream message)
                  (gateway.cable:from-cable stream))))

;;; Server logic

(defvar *identities* (make-hash-table :synchronized t))

(defun echo-server ()
  (loop
    (pzmq:with-message message
      ;; Receive/send identity
      (pzmq:msg-recv message *server-socket*)
      (let* ((identity (cffi:foreign-array-to-lisp
                        (pzmq:msg-data message) `(:array :unsigned-char 5)
                        :element-type '(unsigned-byte 8)))
             (integer (array-to-integer identity)))
        (cond ((gethash integer *identities*)
               (setf (gethash integer *identities*) (local-time:now))
               (format t "~&Adding identity ~S." identity))
              (t
               (format t "~&Receiving from identity ~S." identity)))
        (finish-output))
      ;; Receive/send data
      (pzmq:msg-recv message *server-socket*)
      (let ((string (cffi:foreign-string-to-lisp
                     (pzmq:msg-data message)
                     :count (pzmq:msg-size message))))
        (when (string/= string "")
          (format t "~&Received data ~S." string)
          (loop for integer being the hash-key of *identities*
                for identity = (integer-to-array integer 5)
                do (format t "~&Sending to identity ~S." identity)
                   (finish-output)
                   (cffi:with-pointer-to-vector-data (pointer identity)
                     (pzmq:send *server-socket* pointer :len 5 :sndmore t))
                   (pzmq:send *server-socket* string)))))))

(defun retransmit (from to)
  (pzmq:with-message message
    (pzmq:msg-recv message from)
    (let ((more (pzmq:getsockopt from :rcvmore)))
      (pzmq:msg-send message to :sndmore more))))
