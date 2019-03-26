;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2019
;;;; sql/t/framework.lisp

(in-package #:gateway.sql/test)
(in-readtable protest/parachute)

(defparameter *test-tables-empty-query*
  (let ((pathname (asdf:system-relative-pathname
                   :gateway.sql "t/sql/test-tables-empty.sql")))
    (read-file-into-string pathname)))

(defun test-tables-empty ()
  (loop with result = (pomo:query *test-tables-empty-query*)
        for (name count) in result
        unless (= 0 count)
          do (error "Cleanup failure: table ~S has ~D entries after the test.
~S" name count (pomo:query (uiop:strcat "SELECT * FROM " name)))))

(defun table-empty-p (name)
  (= 0 (pomo:query (uiop:strcat "SELECT COUNT(1) FROM " name) :single)))

(defvar *checked-exports*)
(defvar *exports*)
(defvar *dummy-data* nil)

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
                         (boundp '*checked-exports*)
                         (boundp '*exports*)
                         (member x *exports*))
                (pushnew x *checked-exports*))))
       (serapeum:walk-tree #'walk ',arguments-and-body))))

(defparameter *warn-on-untested-symbols* nil)

(defun test (&rest args)
  (let ((*checked-exports* '())
        (*exports* (compute-exports)))
    (apply #'protest/parachute:test args)
    (when *warn-on-untested-symbols*
      (when-let ((diff (set-difference *exports* *checked-exports*)))
        (warn "~D/~D symbols untested.~{~%~S~}"
              (length diff) (length *exports*) diff)))))

(defmacro with-sql-test (() &body body)
  (with-gensyms (errorp e)
    `(let ((,errorp nil))
       (handler-bind ((error (lambda (,e) (setf ,errorp ,e))))
         (unwind-protect
              (with-test-db ()
                ,@body
                (unless *dummy-data* (test-tables-empty)))
           (when ,errorp
             (with-test-db () (uninstall) (install)
               (when *dummy-data* (install-dummy-data)))))))))

(defmacro db-fail (form &optional description &rest format-args)
  (let ((type 'cl-postgres:database-error))
    `(parachute:eval-in-context
      parachute:*context*
      (make-instance
       'protest/for-parachute:test-case-comparison-result
       :expression '(db-fail ,form)
       :value-form '(capture-error ,form)
       :body (lambda () (parachute::capture-error
                            ,form ,(parachute::maybe-unquote type)))
       :expected ',(parachute::maybe-unquote type)
       :comparison 'typep
       ,@(when description
           `(:description (format NIL ,description ,@format-args)))))))

(defun long-string (n) (make-string n :initial-element #\a))
