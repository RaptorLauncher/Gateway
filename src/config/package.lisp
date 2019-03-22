;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(uiop:define-package #:gateway/config
  (:use #:cl
        #:alexandria
        #:phoe-toolbox
        #:ubiquitous)
  (:export
   #:config
   #:default-config
   #:econfig
   #:remconfig
   #:with-transaction))
