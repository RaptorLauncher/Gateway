;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/objects/package.lisp

(uiop:define-package #:gateway.engine/objects
  (:mix #:list-named-class
        #:cl
        #:alexandria
        #:phoe-toolbox
        #:moptilities
        #:protest/base
        #:protest/common/date
        #:gateway.engine/protocol)
  (:export #:standard-player
           #:standard-date #:now))
