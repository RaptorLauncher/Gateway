;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; engine/protocol/package.lisp

(uiop:define-package #:gateway.engine/protocol
  (:use #:cl)
  (:import-from #:protest/protocol
                #:define-protocol
                #:execute-protocol))
