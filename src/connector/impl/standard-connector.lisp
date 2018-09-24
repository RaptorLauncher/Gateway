;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/impl/standard-connector.lisp

(in-package :gateway.connector)

(defclass standard-connector (connector)
  ((%acceptor :accessor acceptor)
   (%listener :accessor listener)
   (%writer :accessor writer)
   (%acceptors :accessor acceptors)
   (%listeners :accessor listeners)
   (%writers :accessor writers))
  (:documentation #.(format nil "A standard connector implementation with a
single acceptor, listener and writer.")))

(define-print (standard-connector stream)
  (format stream "~A (~:[ALIVE, ~D connections~;DEAD~])"
          (address (acceptor standard-connector))
          (deadp standard-connector)
          (connection-count (listener standard-connector))))

(define-constructor (standard-connector (host "127.0.0.1") (port 0)
                                        acceptor-handler listener-handler
                                        acceptor listener writer)
  (check-type host string)
  (check-type port (unsigned-byte 16))
  (assert (or listener listener-handler) (listener-handler)
          "Must provide a listener handler or a listener.")
  (unless acceptor-handler
    (setf acceptor-handler (acceptor-handler standard-connector)))
  (setf (acceptor standard-connector)
        (or acceptor (make-instance 'standard-acceptor
                                    :host host :port port
                                    :handler acceptor-handler))
        (listener standard-connector)
        (or listener (make-instance 'standard-listener
                                    :handler listener-handler))
        (writer standard-connector)
        (or writer (make-instance 'standard-writer))
        (acceptors standard-connector) (list (acceptor standard-connector))
        (listeners standard-connector) (list (listener standard-connector))
        (writers standard-connector) (list (writer standard-connector)))
  (v:trace '(:gateway :connector) "~A: starting." standard-connector))

(defun acceptor-handler (connector)
  (named-lambda standard-acceptor-handle-connection (connection)
    (add-connection (listener connector) connection)))

(defmethod write-data
    ((connector standard-connector) (connection connection) data)
  (write-data (writer connector) connection data))

(defmethod deadp ((connector standard-connector))
  (and (deadp (acceptor connector))
       (deadp (listener connector))
       (deadp (writer connector))))

(defmethod kill ((connector standard-connector))
  (v:trace '(:gateway :connector) "~A: killed." connector)
  (kill (acceptor connector))
  (kill (listener connector))
  (kill (writer connector))
  (values))
