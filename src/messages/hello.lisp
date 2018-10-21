;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/message/hello.lisp

(in-package #:gateway.engine/message)

(define-protocol-class (:hello) () ())

(defclass (:request :hello) ((:request) (:hello)) ())

(defclass (:response :hello) ((:response) (:hello)) ())

(defclass (:error :hello) ((:error) (:hello)) ())
