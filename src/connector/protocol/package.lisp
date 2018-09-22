;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/protocol/package.lisp

(uiop:define-package #:gateway.connector/protocol
  (:use
   #:cl
   #:protest/common/addressed
   #:protest/common/killable
   #:protest/common/handling
   #:protest/common/named
   #:protest/common/serializable)
  (:reexport
   #:protest/common/addressed
   #:protest/common/killable
   #:protest/common/handling
   #:protest/common/named
   #:protest/common/serializable)
  (:import-from
   #:protest/protocol
   #:define-protocol
   #:execute-protocol))
