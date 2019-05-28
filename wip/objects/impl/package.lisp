;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; objects/impl/package.lisp

(uiop:define-package #:gateway.objects/impl
  (:use #:cl
        #:phoe-toolbox
        #:local-time
        #:gateway.base/protocol
        #:gateway.objects/protocol)
  (:export #:standard-player
           #:standard-date))
