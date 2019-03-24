;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2019
;;;; sql/t/framework.lisp

(in-package #:gateway.sql/test)

(defvar *current-test-checked-exports*)
(defvar *exports*)

(defun compute-exports ()
  (let ((exports (loop with package = (find-package '#:gateway.sql)
                       for x being the external-symbols of package
                       collect x))
        (ignored '(install uninstall reinstall with-db with-test-db)))
    (set-difference exports ignored)))

;;; FIXME: Most of this hack should go away when Shinmera implements running
;;; code around tests in a test suite.

(defmacro define-test (name &body arguments-and-body)
  `(protest/parachute:define-test ,name
     ,@arguments-and-body
     (flet ((walk (x)
              (when (and (symbolp x)
                         (boundp '*current-test-checked-exports*)
                         (boundp '*exports*)
                         (member x *exports*))
                (pushnew x *current-test-checked-exports*))))
       (serapeum:walk-tree #'walk ',arguments-and-body))))

(defun test (&rest args)
  (let ((*current-test-checked-exports* '())
        (*exports* (compute-exports)))
    (apply #'protest/parachute:test args)
    (when-let ((diff (set-difference *exports* *current-test-checked-exports*)))
      (warn "~D/~D symbols untested.~{~%~S~}"
            (length diff) (length *exports*) diff))))
