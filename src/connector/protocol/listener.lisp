;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/protocol/listener.lisp

(in-package #:gateway.connector/protocol)

(define-protocol listener
    (:documentation "The LISTENER protocol describes objects which contain a ~
list of connections and constantly scan them for incoming data, reading it ~
and calling their handler function on them to pass it to other parts of the ~
system.
\
The listener has two handler functions. The message handler function is a ~
two-argument function that accepts a connection object and the data that came ~
from that connection as its arguments and pass it to other parts of the ~
program. The disconnection handler function is an one-argument function that ~
accepts a connection object and is called after that connection dies to inform ~
other parts of the program of the disconnection.
\
The listener, when instantiated, automatically begins handling client ~
connections in a way defined by the implementing class.
\
The HANDLER function from the HANDLING protocol has two possible types, ~
:MESSAGE and :DISCONNECTION. The default is :MESSAGE."
     :tags (:gateway :listener)
     :dependencies (acceptor connection killable named handling)
     :export t)
  (:class listener (killable named handling) ())
  "A listener object. See protocol LISTENER for details."
  (:function add-connection ((listener listener) (connection connection)) t)
  "Adds the connection to the listener and notifies the listener about the
change, so, if the listener is waiting, the next iteration of its ~
functionality begins immediately, including the newly provided connection."
  (:function connection-count ((listener listener)) unsigned-byte)
  "Returns the count of the connections inside the listener.")

(execute-protocol listener)
