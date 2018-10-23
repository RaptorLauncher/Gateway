;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; database/protocol/package.lisp

(uiop:define-package #:gateway.database/protocol
  (:use #:cl
        #:protest/common/killable)
  (:import-from #:protest/protocol
                #:define-protocol
                #:execute-protocol))
