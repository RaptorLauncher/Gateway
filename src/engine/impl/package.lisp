;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; engine/impl/package.lisp

(uiop:define-package #:gateway.engine/impl
  (:use #:cl
        #:phoe-toolbox
        #:protest/common/killable
        #:protest/common/named
        #:protest/common/handling
        #:gateway.engine/protocol
        #:gateway.base/protocol)
  (:export #:standard-engine))
