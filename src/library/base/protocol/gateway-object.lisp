;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; base/protocol/gateway-object.lisp

(in-package #:gateway.base/protocol)

(define-protocol gateway-object
    (:documentation "The GATEWAY-OBJECT protocol describes objects and ~
condition types which are related to the internal functioning of Gateway and ~
may be transmitted over the cable.
\
All Gateway object classes must have unique symbol names, even if symbols ~
naming them have different packages.
\
All concrete Gateway objects (subclasses of GATEWAY-OBJECT) and condition ~
types (subclasses of GATEWAY-CONDITION) must be convertible to and from the ~
cable format by defining methods on the CABLE-CONDITION-USING-CLASS and ~
CONDITION-CABLE functions."
     :tags (:gateway :object :condition)
     :dependencies ()
     :export t)
  (:class gateway-object () ())
  "A Gateway cable-transmittable object. See protocol GATEWAY-OBJECT for ~
details."
  (:condition-type gateway-condition () ())
  "A Gateway cable-transmittable condition type. See protocol GATEWAY-OBJECT ~
for details."
  (:function data-object (data) (or gateway-object gateway-condition))
  "Converts the provided cable data into a Gateway object.
\
This function should call DATA-OBJECT-USING-CLASS using the class ~
found using the head of the data as first argument, and the tail of the data ~
as the second argument.
\
Despite this function being a convenience function, it is not implemented ~
as a part of this protocol to allow for concrete conditions of subtype ~
GATEWAY-CONDITION to be created and signaled as a part of this function's ~
effect."
  (:function data-object-using-class ((class class) data &key)
             (or gateway-object gateway-condition))
  "Converts the provided cable data into a Gateway object of the provided ~
class."
  (:function object-data ((object (or gateway-object gateway-condition))) t)
  "Converts the provided Gateway object into cable data.")

(execute-protocol gateway-object)
