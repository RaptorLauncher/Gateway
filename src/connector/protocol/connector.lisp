;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/protocol/connector.lisp

(in-package #:gateway.connector/protocol)

(define-protocol connector
    (:documentation "The CONNECTOR protocol describes objects which are ~
responsible for routing data between user connections and the game logic. ~
Each connector object contains one or more acceptors, listeners and writers.
\
The default method for WRITE-DATA specialized on connectors simply picks a ~
writer at random from the connector's writers list and calls WRITE-DATA on it. ~
Implementations are encouraged to provide their own optimized methods."
     :tags (:gateway :connector)
     :dependencies (killable named writing acceptor listener writer)
     :export t)
  ;; TODO make sure no SETF functions are exported from all implementations
  (:class connector (killable named writing) ())
  "A connector object. See protocol CONNECTOR for details."
  (:function acceptors ((connector connector)) list)
  "Returns a list of acceptors associated with this connector."
  (:function listeners ((connector connector)) list)
  "Returns a list of listeners associated with this connector."
  (:function writers ((connector connector)) list)
  "Returns a list of writers associated with this connector.")

(execute-protocol connector)

(defmethod write-data ((connector connector) connection data)
  (let ((writers (writers connector)))
    (write-data (elt writers (random (length writers))) connection data)))
