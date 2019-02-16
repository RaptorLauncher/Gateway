;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; objects/protocol/package.lisp

(uiop:define-package #:gateway.objects/protocol
  (:use #:cl
        #:protest/common/date
        #:gateway.base)
  (:reexport #:protest/common/date)
  (:import-from #:protest/protocol
                #:define-protocol
                #:execute-protocol))
