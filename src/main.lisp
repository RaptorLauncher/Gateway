;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; main.lisp

(defpackage #:gateway-user
  (:use :cl))

(in-package #:gateway-user)

(defvar *systems*
  '(:gateway.base
    :gateway.cable
    :gateway.connector
    :gateway.engine))

(defun test-gateway ()
  (dolist (system *systems*)
    (asdf:test-system system)))

;; TODO make sure that we deprecate MAKE-CONDITION and instead use MAKE-INSTANCE
