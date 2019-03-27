;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; main.lisp

(defpackage #:gateway-user
  (:use :cl))

(in-package #:gateway-user)

(defparameter *systems*
  '(:gateway.base
    :gateway.cable
    :gateway.connector
    :gateway.engine
    :gateway.sql))

(defun test-gateway ()
  (dolist (system *systems*)
    (asdf:test-system system)))

;; TODO make sure that we deprecate MAKE-CONDITION and instead use MAKE-INSTANCE
