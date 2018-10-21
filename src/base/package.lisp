;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; base/package.lisp

(uiop:define-package #:gateway.base
  (:use #:gateway.base/protocol
        #:gateway.base/impl)
  (:reexport #:gateway.base/protocol
             #:gateway.base/impl)) ;; TODO documentation for BASE
