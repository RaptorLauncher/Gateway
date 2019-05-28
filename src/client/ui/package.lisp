;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2019
;;;; package.lisp

(defpackage #:gateway.client.ui
  (:use #:cl+qt
        #:alexandria
        #:phoe-toolbox
        #:qtools
        #:named-readtables)
  (:shadowing-import-from #:phoe-toolbox #:split))
