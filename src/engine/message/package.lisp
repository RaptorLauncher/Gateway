;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/message/package.lisp

(uiop:define-package #:gateway.engine/message
    (:mix #:list-named-class
          #:cl
          #:alexandria
          #:phoe-toolbox
          #:moptilities
          #:protest/base
          #:gateway.engine/protocol)
  (:export #:standard-message))
