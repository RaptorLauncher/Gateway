;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/protocol/acceptor.lisp

(in-package #:gateway.connector/protocol)

(define-protocol acceptor
    (:documentation "The ACCEPTOR protocol describes objects which accept ~
incoming client connections (objects of protocol class CONNECTION) and call a ~
handler function on that connection, so it may later be handled by other parts ~
of the program .
\
The handler function is a one-argument function that is meant to accept a ~
connection object as its argument and pass it to other parts of the program.
\
The acceptor, when instantiated, automatically begins handling client ~
connections in a way defined by the implementing class."
     :tags (:acceptor)
     :dependencies (connection killable named addressed handling)
     :export t)
  (:class acceptor (killable named addressed handling) ())
  "An acceptor object. See protocol ACCEPTOR for details.")
