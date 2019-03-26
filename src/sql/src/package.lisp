;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; sql/src/package.lisp

(uiop:define-package #:gateway.sql
  (:use #:common-lisp
        #:alexandria
        #:cl-yesql
        #:phoe-toolbox
        #:gateway/config)
  (:shadowing-import-from #:cl-yesql
                          #:import)
  (:shadowing-import-from #:postmodern
                          #:execute-file)
  (:export #:with-db
           #:with-test-db
           #:install
           #:uninstall
           #:install-dummy-data))
