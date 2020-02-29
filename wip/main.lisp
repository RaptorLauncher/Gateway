;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; main.lisp

;; (ql:quickload '(:alexandria :protest/parachute :gateway.init))

(defpackage #:gateway-user
  (:use #:cl #:alexandria)
  (:import-from #:protest/parachute #:test)
  (:import-from #:parachute #:results-with-status)
  (:import-from #:gateway.init #:gateway-full-test)
  (:export #:test-gateway))

(in-package #:gateway-user)

(defparameter *systems*
  '(:gateway.init
    :gateway.base/test
    :gateway.cable/test
    ;; :gateway.connector/test
    :gateway.engine/test
    :gateway.sql/test))

(defun test-gateway (&optional (report 'parachute:plain))
  (mapc #'ql:quickload *systems*)
  (test 'gateway.init:gateway-full-test :report report))

;; TODO make sure that we deprecate MAKE-CONDITION and instead use MAKE-INSTANCE
