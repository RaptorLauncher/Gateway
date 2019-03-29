;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; main.lisp

(defpackage #:gateway-user
  (:use #:cl)
  (:export #:test-gateway))

(in-package #:gateway-user)

(defparameter *systems*
  '(:gateway.init
    :gateway.base/test
    :gateway.cable/test
    :gateway.connector/test
    :gateway.engine/test
    :gateway.sql/test))

(defun test-gateway ()
  (mapc #'asdf:load-system *systems*)
  (let ((result (protest/parachute:test 'gateway.init:gateway-full-test)))
    (when (parachute:results-with-status :failed result)
      (error "There are test failures."))))

;; TODO make sure that we deprecate MAKE-CONDITION and instead use MAKE-INSTANCE
