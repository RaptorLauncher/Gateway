;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; cable/input.lisp

(in-package #:gateway.cable)

(defun cable-equal (x y)
  "Like EQUAL, except it returns T for gensyms with the same name."
  (declare (optimize speed))
  (labels ((%cable-equal (x y)
             (typecase x
               (symbol (and (symbolp y)
                            (eq (symbol-package x)
                                (symbol-package y))))
               (cons (and (consp y)
                          (%cable-equal (car x) (car y))
                          (%cable-equal (cdr x) (cdr y))))
               (t (equal x y)))))
    (%cable-equal x y)))
