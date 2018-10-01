;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; engine/protocol/message.lisp

(in-package #:gateway.engine/protocol)

(define-protocol message
    (:documentation "The MESSAGE protocol describes objects which represent ~
internal messages in the Gateway system. Each message consists of a message ~
ID, which is a two-element list containing either :CLIENT or :SERVER and a ~
non-negative integer, a body, which is data transmittable by the cable ~
protocol, and a state, which describes the context of the message body: if the ~
message was a request, if it is a successful response, or a response ~
denoting an error of some sort."
     :tags (:gateway :engine :message)
     :dependencies ()
     :export t)
  (:class message () ())
  "A message object. See protocol MESSAGE for details."
  (:function id ((message message))
             (cons (member :client :server) (cons unsigned-byte null)))
  "Returns the ID of the message."
  (:function status ((message message)) (member :request :ok :error))
  "Returns the message's statis."
  (:function data-message (data) message)
  "Converts the provided cable data into a message.
\
This function should must call DATA-MESSAGE-USING-CLASS after performing all ~
checks on the received data."
  (:function data-message-using-class ((class class) id status data) message)
  "Converts the provided cable data into a message of the provided class."
  (:function message-data ((message message)) t)
  "Converts the provided message into cable data."
  (:condition-type message-condition (gateway-condition) ())
  "A condition type related to processing Gateway messages.")

(execute-protocol message)
