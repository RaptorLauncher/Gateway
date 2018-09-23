;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/impl/standard-connection.lisp

(in-package :gateway.connector)
;;(in-readtable protest)

(defclass standard-connection (connection usocket:stream-usocket)
  ((%address :accessor address))
  (:documentation #.(format nil "A standard implementation of Gateway protocol ~
class CONNECTION.
\
This connection is a subclass of USOCKET:STREAM-SOCKET.")))

(define-print (standard-connection stream)
  (format stream "~A (~A)" (address standard-connection)
          (if (deadp standard-connection) "DEAD" "ALIVE")))

(defun %initialize-connection (connection)
  (let ((address (socket-peer-address connection)))
    (v:trace '(:gateway :connection)
             "Standard connection created for ~A." address)
    (setf (address connection) address)))

(define-constructor (standard-connection)
  (%initialize-connection standard-connection))

(defmethod update-instance-for-different-class
    ((socket usocket:stream-usocket) (connection connection) &key)
  (call-next-method)
  (%initialize-connection connection))

(defmethod deadp ((connection standard-connection))
  (handler-case (peek-char-no-hang (usocket:socket-stream connection))
    (error () (usocket:socket-close connection)))
  (not (open-stream-p (usocket:socket-stream connection))))

(defmethod kill ((connection standard-connection))
  (usocket:socket-close connection)
  (v:trace '(:gateway :connection)
           "Standard connection killed for ~A." (address connection))
  (values))

(defun connection-readyp (connection)
  (handler-case (peek-char-no-hang (usocket:socket-stream connection))
    (end-of-file () t)))

(defmethod readyp ((connection standard-connection))
  (or (not (open-stream-p (usocket:socket-stream connection)))
      (connection-readyp connection)))

(defmethod connection-receive ((connection standard-connection))
  (cond ((deadp connection) (values nil nil))
        ((not (connection-readyp connection)) (values nil t))
        (t (let ((result (from-cable (usocket:socket-stream connection))))
             (v:trace '(:gateway :connection)
                      "Received ~S from ~A." result connection)
             (values result t)))))

(defmethod connection-send ((connection standard-connection) object)
  (let ((stream (usocket:socket-stream connection)))
    (v:trace '(:gateway :connection)
             "Sending ~S to ~A." object connection)
    (to-cable object stream)
    (terpri stream)
    (force-output stream)))

(defun %ready (connections)
  (usocket:wait-for-input connections :timeout 0.1 :ready-only t))

(defmethod ready-connection-using-class
    ((class (eql (find-class 'standard-connection))) connections)
  (let ((connections connections))
    (loop
      (handler-case
          (let* ((connection (first (%ready connections))))
            (v:trace '(:gateway :connection)
                     "Connection ~A is ready." connection)
            (return (values connection connections)))
        ((or stream-error usocket:socket-error) ()
          (setf connections (remove-if #'deadp connections))
          ;; TODO make use of secondary value in listener
          (unless connections (return (values nil nil))))))))

;;; TESTS

;; (define-test-case standard-connection-send-receive
;;     (:description "Test of sending and receiving data for STANDARD-CONNECTIONs."
;;      :tags (:connection :protocol)
;;      :type :protocol)
;;   :arrange
;;   1 "Create connection 1 (server)."
;;   2 "Create connection 2 (client)."
;;   :act
;;   3 "Send test data from connection 1."
;;   :assert
;;   4 "Assert connection 2 is ready."
;;   5 "Assert the received data matches the data that was sent."
;;   :act
;;   6 "Send test data from connection 2."
;;   :assert
;;   7 "Assert connection 3 is ready."
;;   8 "Assert the received data matches the data that was sent.")

;; (define-test standard-connection-send-receive
;;   (finalized-let*
;;       ((socket-listen (socket-listen "127.0.0.1" 0)
;;                       (socket-close socket-listen))
;;        (port (get-local-port socket-listen))
;;        (connection-1 #1?(make-instance 'standard-connection :port port)
;;                      (kill connection-1))
;;        (socket-accept (socket-accept socket-listen))
;;        (connection-2 #2?(make-instance 'standard-connection
;;                                        :socket socket-accept)
;;                      (kill connection-2)))
;;     (let ((test-cases '((1 2 3 4 5 6 7 8 9 0)
;;                         (#:a #:b #:c #:d #:e #:f (#:g)
;;                          ((((#:h #:i #:j #:k (#:l 2000) #:m #:n)))))
;;                         (#:lorem #:ipsum #:dolor #:sit #:amet)
;;                         ("a" #:a "a" #:a "a" "b"))))
;;       (labels ((test-case (x y data)
;;                  #3?(connection-send x data)
;;                  #4?(is (wait () (readyp y)))
;;                  #5?(is (data-equal data (connection-receive y)))
;;                  #6?(connection-send y data)
;;                  #7?(is (wait () (readyp x)))
;;                  #8?(is (data-equal data (connection-receive x)))))
;;         (mapc (alexandria:curry #'test-case connection-1 connection-2)
;;               test-cases)))))

;; (define-test-case standard-connection-death
;;     (:description "Test of KILLABLE protocol for STANDARD-CONNECTIONs."
;;      :tags (:protocol :killable :connection)
;;      :type :protocol)
;;   :arrange
;;   1 "Create connections."
;;   2 "Assert connection 1 is alive."
;;   3 "Assert connection 2 is alive."
;;   :act
;;   4  "Kill connection 1."
;;   :assert
;;   5 "Assert connection 1 is dead."
;;   6 "Assert connection 2 is dead.")

;; (define-test standard-connection-death
;;   (finalized-let*
;;       ((conns (multiple-value-list #1?(make-connection-pair))
;;               (mapc #'kill conns)))
;;     #2?(is (alivep (first conns)))
;;     #3?(is (alivep (second conns)))
;;     #4?(kill (first conns))
;;     #5?(is (deadp (first conns)))
;;     #6?(is (deadp (second conns)))))
