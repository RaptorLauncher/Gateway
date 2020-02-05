;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2019
;;;; package.lisp

(defpackage #:gateway.client.ui
  (:use #:cl+qt
        #:alexandria
        #:phoe-toolbox
        #:split-sequence
        #:qtools
        #:named-readtables)
  (:shadow #:stream)
  (:shadowing-import-from #:phoe-toolbox #:split))
