;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; engine/protocol/package.lisp

(uiop:define-package #:gateway.engine/protocol
  (:use #:cl
        #:protest/common/killable
        #:protest/common/handling
        #:protest/common/named
        #:gateway.connector/protocol
        #:gateway.base/protocol)
  (:reexport #:protest/common/killable
             #:protest/common/handling
             #:protest/common/named)
  (:import-from #:protest/protocol
                #:define-protocol
                #:execute-protocol))
