;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; engine/protocol/message.lisp

(in-package #:gateway.engine/protocol)

(define-protocol message
    (:documentation "The MESSAGE protocol describes objects which represent ~
internal messages in the Gateway system. Each message consists of a message ~
body, which is data transmittable by the cable protocol, and a state, which ~
describes the context of the message body: if the message was a request, if ~
it is a successful response, or a response signifying an error of some sort."
     :tags (:gateway :engine :message)
     :dependencies ()
     :export t)
  (:class message () ())
  "A message object. See protocol MESSAGE for details."
  (:function data ((message message)) t)
  "Returns the message's data."
  (:function state ((message message)) (member :request :ok :error))
  "Returns the message's state."
  (:function data-message (data) message)
  "Converts the provided cable data into a message."
  (:function message-data ((message message)) t)
  "Converts the provided message into cable data.")
