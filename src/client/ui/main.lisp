;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2020
;;;; package.lisp

(in-package :gateway.client.ui)
(in-readtable :qtools)

(defun main ()
  (with-main-window (window (q+:make-qlabel "Hello world!"))
    (setf (q+:minimum-height window) 300
          (q+:minimum-width window) 400)))
