;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; base/protocol/package.lisp

(uiop:define-package #:gateway.base/protocol
  (:use #:cl
        #:protest/common/date)
  (:import-from #:protest/protocol
                #:define-protocol
                #:execute-protocol))
