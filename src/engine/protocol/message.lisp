;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; engine/protocol/message.lisp

(in-package #:gateway.engine/protocol)

(define-protocol message
    (:documentation "The MESSAGE protocol describes objects which represent ~
internal messages in the Gateway system. Each message consists of a message ~
ID, which is a two-element list containing either :CLIENT or :SERVER and a ~
non-negative integer, and the message body, which is data transmittable by the ~
cable protocol.
\
The name of each message class is a two-element list, as defined by the ~
LIST-NAMED-CLASS Lisp system. The first element is one of :REQUEST, :RESPONSE ~
or :ERROR, and the second element is any keyword."
     :tags (:gateway :engine :message)
     :dependencies (gateway-object) ;; TODO engine as a dependency
     :export t)
  (:class message (gateway-object) ())
  "A message object. See protocol MESSAGE for details."
  (:function id ((message message))
             (cons (member :client :server) (cons unsigned-byte null)))
  "Returns the ID of the message."
  ;; TODO move this to the engine; it's the engine's responsibility to execute
  ;; messages.
  (:function execute-message ((message message) (engine #|engine|# t)) (values))
  "Executes the provided message on the given engine."
  (:condition-type message-condition (gateway-condition) ())
  "A condition type related to processing Gateway messages.")

(execute-protocol message)
