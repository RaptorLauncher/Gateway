(ql:quickload '(:pzmq :gateway.cable))

;; Utils

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

;; Client networking

(defparameter *client-context* nil)

(defparameter *client-socket* nil)

(defun start-client (&optional (server-address "tcp://localhost:6500"))
  (setf *client-context* (pzmq:ctx-new))
  (setf *client-socket* (pzmq:socket *client-context* :dealer))
  (set-socket-options *client-socket*
                      :linger 100)
  (pzmq:connect *client-socket* server-address))

(defun stop-client ()
  (kill-socket *client-socket*)
  (kill-context *client-context*))

;; Client logic

(defun ping ()
  (format t "~&Sending ping...")
  (let ((data '(ping (1 2 :hello "foo" 3))))
    (print data)
    (pzmq:send *client-socket*
               (with-output-to-string (stream) (gateway.cable:to-cable data stream))))
  (format t "~&Receiving...")
  (print (with-input-from-string (stream (pzmq:recv-string *client-socket*))
           (gateway.cable:from-cable stream))))

;; Server networking

(defparameter *server-context* nil)

(defparameter *server-socket* nil)

(defun stop-server ()
  (kill-socket *server-socket*)
  (kill-context *server-context*))

(defun start-server (&optional (listen-address "tcp://*:6500"))
  (setf *server-context* (pzmq:ctx-new))
  (setf *server-socket* (pzmq:socket *server-context* :router))
  (set-socket-options *server-socket*
                      :immediate 1
                      :router-handover 1
                      :heartbeat-ivl 10000
                      :connect-timeout 30000
                      :heartbeat-timeout 30000)
  (pzmq:bind *server-socket* listen-address))

;; Server logic

(defun echo-server ()
  (loop
    (let ((socket *server-socket*))
      (pzmq:with-message message
        (pzmq:msg-recv message socket)
        (let ((identity (cffi:foreign-array-to-lisp
                         (pzmq:msg-data message) `(:array :unsigned-char 5)
                         :element-type '(unsigned-byte 8))))
          (pzmq:msg-recv message socket)
          (let ((data (cffi:foreign-string-to-lisp
                       (pzmq:msg-data message)
                       :count (pzmq:msg-size message))))
            (cffi:with-pointer-to-vector-data (pointer identity)
              (pzmq:send socket pointer :len 5 :sndmore t))
            (let ((request (with-input-from-string (stream data)
                             (gateway.cable:from-cable stream)))
                  (response nil))
              (if (eq (first request) 'ping)
                  (setf response `(pong ,(second request)))
                  (setf response `(huh? ,request)))
              (pzmq:send socket
                         (with-output-to-string (stream)
                           (gateway.cable:to-cable response stream))))))))))

(defun retransmit (from to)
  (pzmq:with-message message
    (pzmq:msg-recv message from)
    (let ((more (pzmq:getsockopt from :rcvmore)))
      (pzmq:msg-send message to :sndmore more))))
