;; Client networking

(defparameter *client-context* nil)

(defparameter *client-socket* nil)

(defun start-client (&optional (server-address "tcp://localhost:5555"))
  (let ((context (pzmq:ctx-new)))
    (setf *client-context* context)
    (let* ((socket (pzmq:socket context :dealer)))
      (pzmq:setsockopt socket :linger 100)
      (pzmq:connect socket server-address)
      (setf *client-socket* socket))))

(defun stop-client ()
  (when *client-socket*
    (pzmq:close *client-socket*)
    (setf *client-socket* nil))
  (when *client-context*
    (pzmq:ctx-term *client-context*)
    (setf *client-context* nil)))



;; Client logic

(defun ping ()
  (format t "~&Sending ping...")
  (let ((data '(ping (1 2 :hello "foo" 3))))
    (print data)
    (pzmq:send *client-socket* (prin1-to-string data)))
  (format t "~&Receiving...")
  (print (read-from-string (pzmq:recv-string *client-socket*))))



;; Server networking

(defparameter *server-context* nil)

(defparameter *server-socket* nil)

(defun start-server (&optional (listen-address "tcp://*:5555"))
  (let ((context (pzmq:ctx-new)))
    (setf *server-context* context)
    (let* ((socket (pzmq:socket context :router)))
      ;; Wait for PZMQ to catch up with these
      ;; (pzmq:setsockopt socket :router-handover 1)
      ;; (pzmq:setsockopt socket :hearbeat-ivl 10000)
      ;; (pzmq:setsockopt socket :hearbeat-timeout 30000)
      (pzmq:bind socket listen-address)
      (setf *server-socket* socket))))

(defun stop-server ()
  (when *server-socket*
    (pzmq:close *server-socket*)
    (setf *server-socket* nil))
  (when *server-context*
    (pzmq:ctx-term *server-context*)
    (setf *server-context* nil)))



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
            (let ((request (read-from-string data))
                  (response nil))
              (if (eq (first request) 'ping)
                  (setf response `(pong ,(second request)))
                  (setf response `(huh? ,request)))
              (pzmq:send socket (prin1-to-string response)))))))))

(defun retransmit (from to)
  (pzmq:with-message message
    (pzmq:msg-recv message from)
    (let ((more (pzmq:getsockopt from :rcvmore)))
      (pzmq:msg-send message to :sndmore more))))
